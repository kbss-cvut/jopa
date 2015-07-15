package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.CacheManager;

import java.net.URI;
import java.util.*;
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

    private Set<Class<?>> inferredClasses;

    public LruCacheManager() {
        this(Collections.emptyMap());
    }

    public LruCacheManager(Map<String, String> properties) {
        Objects.requireNonNull(properties);
        this.capacity = properties.containsKey(OWLAPIPersistenceProperties.LRU_CACHE_CAPACITY) ?
                resolveCapacitySetting(properties) : DEFAULT_CAPACITY;
    }

    private int resolveCapacitySetting(Map<String, String> properties) {
        int capacitySetting = DEFAULT_CAPACITY;
        try {
            capacitySetting =
                    Integer.parseInt(properties.get(OWLAPIPersistenceProperties.LRU_CACHE_CAPACITY));
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

    }

    @Override
    public <T> T get(Class<T> cls, Object primaryKey, URI context) {
        return null;
    }

    @Override
    public void clearInferredObjects() {

    }

    @Override
    public void setInferredClasses(Set<Class<?>> inferredClasses) {
        this.inferredClasses = inferredClasses;
    }

    @Override
    public void close() {

    }

    @Override
    public boolean contains(Class<?> cls, Object primaryKey) {
        return false;
    }

    @Override
    public boolean contains(Class<?> cls, Object primaryKey, URI context) {
        return false;
    }

    @Override
    public void evict(Class<?> cls, Object primaryKey, URI context) {

    }

    @Override
    public void evict(Class<?> cls) {

    }

    @Override
    public void evict(URI context) {

    }

    @Override
    public void evictAll() {

    }

    private static final class CacheNode {
        private Class<?> cls;
        private Object identifier;
        private URI context;

        private CacheNode(Class<?> cls, Object identifier, URI context) {
            this.cls = cls;
            this.identifier = identifier;
            this.context = context;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            CacheNode cacheNode = (CacheNode) o;

            return cls.equals(cacheNode.cls) && identifier.equals(cacheNode.identifier) && context
                    .equals(cacheNode.context);

        }

        @Override
        public int hashCode() {
            int result = cls.hashCode();
            result = 31 * result + identifier.hashCode();
            result = 31 * result + context.hashCode();
            return result;
        }
    }

    // TODO The cache will be a standard cache with map of context -> map of class -> map of identifier -> object
    // Plus there will be a LinkedHashMap for maintaining the LRU policy. Once an object is too old, it will be evicted
}
