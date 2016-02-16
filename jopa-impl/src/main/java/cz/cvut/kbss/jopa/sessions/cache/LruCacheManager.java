package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

import java.net.URI;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;
import java.util.logging.Logger;

/**
 * This is a fixed-size second level cache implementation with LRU eviction policy.
 * <p>
 * When the capacity is reached, the least recently used entry is removed from the cache.
 *
 * @author kidney
 */
public class LruCacheManager implements CacheManager {

    private static final Logger LOG = Logger.getLogger(LruCacheManager.class.getName());

    /**
     * Default cache size limit in number of entries.
     */
    public static final int DEFAULT_CAPACITY = 512;

    private final int capacity;

    private final Lock readLock;
    private final Lock writeLock;

    private LruEntityCache entityCache;

    private Set<Class<?>> inferredClasses;

    public LruCacheManager() {
        this(Collections.emptyMap());
    }

    public LruCacheManager(Map<String, String> properties) {
        Objects.requireNonNull(properties);
        this.capacity = properties.containsKey(JOPAPersistenceProperties.LRU_CACHE_CAPACITY) ?
                resolveCapacitySetting(properties) : DEFAULT_CAPACITY;
        final ReadWriteLock rwLock = new ReentrantReadWriteLock();
        this.readLock = rwLock.readLock();
        this.writeLock = rwLock.writeLock();
        this.entityCache = new LruEntityCache(capacity);
    }

    private int resolveCapacitySetting(Map<String, String> properties) {
        int capacitySetting = DEFAULT_CAPACITY;
        try {
            capacitySetting =
                    Integer.parseInt(properties.get(JOPAPersistenceProperties.LRU_CACHE_CAPACITY));
            if (capacitySetting <= 0) {
                LOG.warning("Invalid LRU cache capacity value " + capacitySetting + ". Using default value.");
                capacitySetting = DEFAULT_CAPACITY;
            }
        } catch (NumberFormatException e) {
            LOG.severe("Unable to parse LRU cache capacity setting. Using default capacity " + DEFAULT_CAPACITY);
        }
        return capacitySetting;
    }

    int getCapacity() {
        return capacity;
    }

    @Override
    public void add(Object primaryKey, Object entity, URI context) {
        Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));

        writeLock.lock();
        try {
            entityCache.put(primaryKey, entity, context);
        } finally {
            writeLock.unlock();
        }
    }

    @Override
    public <T> T get(Class<T> cls, Object primaryKey, URI context) {
        if (cls == null || primaryKey == null) {
            return null;
        }
        readLock.lock();
        try {
            return entityCache.get(cls, primaryKey, context);
        } finally {
            readLock.unlock();
        }
    }

    @Override
    public void clearInferredObjects() {
        writeLock.lock();
        try {
            getInferredClasses().forEach(this::evict);
        } finally {
            writeLock.unlock();
        }
    }

    private Set<Class<?>> getInferredClasses() {
        if (inferredClasses == null) {
            return Collections.emptySet();
        }
        return inferredClasses;
    }

    @Override
    public void setInferredClasses(Set<Class<?>> inferredClasses) {
        this.inferredClasses = inferredClasses;
    }

    @Override
    public void close() {
        // No-op
    }

    @Override
    public boolean contains(Class<?> cls, Object primaryKey) {
        if (cls == null || primaryKey == null) {
            return false;
        }
        readLock.lock();
        try {
            return entityCache.contains(cls, primaryKey);
        } finally {
            readLock.unlock();
        }
    }

    @Override
    public boolean contains(Class<?> cls, Object primaryKey, URI context) {
        if (cls == null || primaryKey == null) {
            return false;
        }
        readLock.lock();
        try {
            return entityCache.contains(cls, primaryKey, context);
        } finally {
            readLock.unlock();
        }
    }

    @Override
    public void evict(Class<?> cls, Object primaryKey, URI context) {
        Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
        Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));

        writeLock.lock();
        try {
            entityCache.evict(cls, primaryKey, context);
        } finally {
            writeLock.unlock();
        }
    }

    @Override
    public void evict(Class<?> cls) {
        Objects.requireNonNull(cls);

        writeLock.lock();
        try {
            entityCache.evict(cls);
        } finally {
            writeLock.unlock();
        }
    }

    @Override
    public void evict(URI context) {
        writeLock.lock();
        try {
            entityCache.evict(context);
        } finally {
            writeLock.unlock();
        }
    }

    @Override
    public void evictAll() {
        writeLock.lock();
        try {
            this.entityCache = new LruEntityCache(capacity);
        } finally {
            writeLock.unlock();
        }
    }

    static final class LruEntityCache extends EntityCache implements Consumer<LruCache.CacheNode> {

        private static final Object NULL_VALUE = null;

        private final LruCache cache;

        LruEntityCache(int capacity) {
            this.cache = new LruCache(capacity, this);
        }

        @Override
        public void accept(LruCache.CacheNode cacheNode) {
            super.evict(cacheNode.getCls(), cacheNode.getIdentifier(), cacheNode.getContext());
        }

        @Override
        void put(Object primaryKey, Object entity, URI context) {
            final URI ctx = context != null ? context : defaultContext;
            super.put(primaryKey, entity, ctx);
            cache.put(new LruCache.CacheNode(ctx, entity.getClass(), primaryKey), NULL_VALUE);
        }

        @Override
        <T> T get(Class<T> cls, Object primaryKey, URI context) {
            final URI ctx = context != null ? context : defaultContext;
            T result = super.get(cls, primaryKey, ctx);
            if (result != null) {
                cache.get(new LruCache.CacheNode(ctx, cls, primaryKey));
            }
            return result;
        }

        @Override
        void evict(Class<?> cls, Object primaryKey, URI context) {
            final URI ctx = context != null ? context : defaultContext;
            super.evict(cls, primaryKey, ctx);
            cache.remove(new LruCache.CacheNode(ctx, cls, primaryKey));
        }

        @Override
        void evict(URI context) {
            if (context == null) {
                context = defaultContext;
            }
            if (!repoCache.containsKey(context)) {
                return;
            }
            final Map<Class<?>, Map<Object, Object>> ctxContent = repoCache.get(context);
            for (Map.Entry<Class<?>, Map<Object, Object>> e : ctxContent.entrySet()) {
                for (Object id : e.getValue().keySet()) {
                    cache.remove(new LruCache.CacheNode(context, e.getKey(), id));
                }
            }
            ctxContent.clear();
        }

        @Override
        void evict(Class<?> cls) {
            for (Map.Entry<URI, Map<Class<?>, Map<Object, Object>>> e : repoCache.entrySet()) {
                final Map<Class<?>, Map<Object, Object>> m = e.getValue();
                final Map<Object, Object> cached = m.remove(cls);
                if (cached != null) {
                    for (Object id : cached.keySet()) {
                        cache.remove(new LruCache.CacheNode(e.getKey(), cls, id));
                    }
                }
            }
        }
    }
}
