/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;

/**
 * This is a fixed-size second level cache implementation with LRU eviction policy.
 * <p>
 * When the capacity is reached, the least recently used entry is removed from the cache.
 */
public class LruCacheManager implements CacheManager {

    private static final Logger LOG = LoggerFactory.getLogger(LruCacheManager.class);

    /**
     * Default cache size limit in number of entries.
     */
    public static final int DEFAULT_CAPACITY = 512;

    private final int capacity;

    private final Lock readLock;
    private final Lock writeLock;

    private LruEntityCache entityCache;

    private Set<Class<?>> inferredClasses;

    LruCacheManager() {
        this(Collections.emptyMap());
    }

    LruCacheManager(Map<String, String> properties) {
        Objects.requireNonNull(properties);
        this.capacity = properties.containsKey(JOPAPersistenceProperties.LRU_CACHE_CAPACITY) ?
                resolveCapacitySetting(properties) : DEFAULT_CAPACITY;
        final ReadWriteLock rwLock = new ReentrantReadWriteLock();
        this.readLock = rwLock.readLock();
        this.writeLock = rwLock.writeLock();
        this.entityCache = new LruEntityCache(capacity);
    }

    private static int resolveCapacitySetting(Map<String, String> properties) {
        int capacitySetting = DEFAULT_CAPACITY;
        try {
            capacitySetting = Integer.parseInt(properties.get(JOPAPersistenceProperties.LRU_CACHE_CAPACITY));
            if (capacitySetting <= 0) {
                LOG.warn("Invalid LRU cache capacity value {}. Using default value.", capacitySetting);
                capacitySetting = DEFAULT_CAPACITY;
            }
        } catch (NumberFormatException e) {
            LOG.error("Unable to parse LRU cache capacity setting. Using default capacity {}.", DEFAULT_CAPACITY);
        }
        return capacitySetting;
    }

    int getCapacity() {
        return capacity;
    }

    @Override
    public void add(Object primaryKey, Object entity, Descriptor descriptor) {
        Objects.requireNonNull(primaryKey, ErrorUtils.getNPXMessageSupplier("primaryKey"));
        Objects.requireNonNull(entity, ErrorUtils.getNPXMessageSupplier("entity"));
        Objects.requireNonNull(descriptor, ErrorUtils.getNPXMessageSupplier("descriptor"));

        writeLock.lock();
        try {
            entityCache.put(primaryKey, entity, descriptor);
        } finally {
            writeLock.unlock();
        }
    }

    @Override
    public <T> T get(Class<T> cls, Object primaryKey, Descriptor descriptor) {
        if (cls == null || primaryKey == null || descriptor == null) {
            return null;
        }
        readLock.lock();
        try {
            return entityCache.get(cls, primaryKey, descriptor);
        } finally {
            readLock.unlock();
        }
    }

    @Override
    public void evictInferredObjects() {
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
        evictAll();
    }

    @Override
    public boolean contains(Class<?> cls, Object identifier, Descriptor descriptor) {
        if (cls == null || identifier == null || descriptor == null) {
            return false;
        }
        readLock.lock();
        try {
            return entityCache.contains(cls, identifier, descriptor);
        } finally {
            readLock.unlock();
        }
    }

    @Override
    public void evict(Class<?> cls, Object identifier, URI context) {
        Objects.requireNonNull(cls, ErrorUtils.getNPXMessageSupplier("cls"));
        Objects.requireNonNull(identifier, ErrorUtils.getNPXMessageSupplier("primaryKey"));

        writeLock.lock();
        try {
            entityCache.evict(cls, identifier, context);
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
        void put(Object identifier, Object entity, Descriptor descriptor) {
            if (!isCacheable(descriptor)) {
                return;
            }
            final URI ctx = descriptor.getSingleContext().orElse(defaultContext);
            super.put(identifier, entity, descriptor);
            cache.put(new LruCache.CacheNode(ctx, entity.getClass(), identifier), NULL_VALUE);
        }

        @Override
        <T> T get(Class<T> cls, Object identifier, Descriptor descriptor) {
            return getInternal(cls, identifier, descriptor,
                    ctx -> cache.get(new LruCache.CacheNode(ctx, cls, identifier)));
        }

        @Override
        void evict(Class<?> cls, Object identifier, URI context) {
            final URI ctx = context != null ? context : defaultContext;
            super.evict(cls, identifier, ctx);
            cache.remove(new LruCache.CacheNode(ctx, cls, identifier));
        }

        @Override
        void evict(URI context) {
            final URI ctx = context != null ? context : defaultContext;
            if (!repoCache.containsKey(ctx)) {
                return;
            }
            final Map<Object, Map<Class<?>, Object>> ctxContent = repoCache.get(ctx);
            for (Map.Entry<Object, Map<Class<?>, Object>> e : ctxContent.entrySet()) {
                e.getValue().forEach((cls, instance) -> {
                    descriptors.remove(instance);
                    cache.remove(new LruCache.CacheNode(ctx, cls, e.getKey()));
                });
            }
            ctxContent.clear();
            // Remove the whole context map
            repoCache.remove(ctx);
        }

        @Override
        void evict(Class<?> cls) {
            final Iterator<Map.Entry<URI, Map<Object, Map<Class<?>, Object>>>> repoIt = repoCache.entrySet().iterator();
            while (repoIt.hasNext()) {
                final Map.Entry<URI, Map<Object, Map<Class<?>, Object>>> e = repoIt.next();
                final URI ctx = e.getKey();
                final Iterator<Map.Entry<Object, Map<Class<?>, Object>>> it = e.getValue().entrySet().iterator();
                while (it.hasNext()) {
                    final Map.Entry<Object, Map<Class<?>, Object>> idEntry = it.next();
                    final Object instance = idEntry.getValue().remove(cls);
                    if (instance != null) {
                        descriptors.remove(instance);
                    }
                    cache.remove(new LruCache.CacheNode(ctx, cls, idEntry.getKey()));
                    // Remove the whole identifier-based map if the removed node was the last one
                    if (idEntry.getValue().isEmpty()) {
                        it.remove();
                    }
                }
                // Remove the whole context map if the removed node was the last one
                if (e.getValue().isEmpty()) {
                    repoIt.remove();
                }
            }
        }
    }
}
