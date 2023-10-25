/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
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
import java.util.Map.Entry;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * Manages the second level cache shared by all persistence contexts.
 * <p>
 * This implementation of CacheManager uses cache-wide locking, i. e. the whole cache is locked when an entity is being
 * put in it, no matter that only one context is affected by the change.
 * <p>
 * This cache is swept regularly by a dedicated thread, which removes all entries whose time-to-live (TTL) has
 * expired.
 */
public class TtlCacheManager implements CacheManager {

    private static final Logger LOG = LoggerFactory.getLogger(TtlCacheManager.class);

    /**
     * Default time to live in millis
     */
    private static final long DEFAULT_TTL = 60000L;
    // Initial delay is the sweep rate multiplied by this multiplier
    private static final int DELAY_MULTIPLIER = 2;
    /**
     * Default sweep rate in millis
     */
    private static final long DEFAULT_SWEEP_RATE = 30000L;

    private Set<Class<?>> inferredClasses;

    private TtlCache cache;

    // Each repository can have its own lock and they could be acquired by this
    // instance itself, no need to pass this burden to callers
    private final Lock readLock;
    private final Lock writeLock;

    private final ScheduledExecutorService sweeperScheduler;
    private Future<?> sweeperFuture;
    private long initDelay;
    private long sweepRate;
    private long timeToLive;
    private volatile boolean sweepRunning;

    public TtlCacheManager(Map<String, String> properties) {
        this.cache = new TtlCache();
        initSettings(properties);
        final ReentrantReadWriteLock lock = new ReentrantReadWriteLock();
        this.readLock = lock.readLock();
        this.writeLock = lock.writeLock();
        this.sweeperScheduler = Executors.newSingleThreadScheduledExecutor();
        this.sweeperFuture = sweeperScheduler.scheduleAtFixedRate(new CacheSweeper(), initDelay, sweepRate,
                TimeUnit.MILLISECONDS);
    }

    private void initSettings(Map<String, String> properties) {
        if (!properties.containsKey(JOPAPersistenceProperties.CACHE_TTL)) {
            this.timeToLive = DEFAULT_TTL;
        } else {
            final String strCacheTtl = properties.get(JOPAPersistenceProperties.CACHE_TTL);
            try {
                // The property is in seconds, we need milliseconds
                this.timeToLive = Long.parseLong(strCacheTtl) * 1000;
            } catch (NumberFormatException e) {
                LOG.warn("Unable to parse cache time to live setting value {}, using default value.", strCacheTtl);
                this.timeToLive = DEFAULT_TTL;
            }
        }
        if (!properties.containsKey(JOPAPersistenceProperties.CACHE_SWEEP_RATE)) {
            this.sweepRate = DEFAULT_SWEEP_RATE;
        } else {
            final String strSweepRate = properties
                    .get(JOPAPersistenceProperties.CACHE_SWEEP_RATE);
            try {
                // The property is in seconds, we need milliseconds
                this.sweepRate = Long.parseLong(strSweepRate) * 1000;
            } catch (NumberFormatException e) {
                LOG.warn("Unable to parse sweep rate setting value {}, using default value.", strSweepRate);
                this.sweepRate = DEFAULT_SWEEP_RATE;
            }
        }
        this.initDelay = DELAY_MULTIPLIER * sweepRate;
    }

    @Override
    public void close() {
        if (sweeperFuture != null) {
            LOG.debug("Stopping cache sweeper.");
            sweeperFuture.cancel(true);
            sweeperScheduler.shutdown();
        }
        this.sweeperFuture = null;
        evictAll();
    }

    @Override
    public void add(Object primaryKey, Object entity, Descriptor descriptor) {
        Objects.requireNonNull(primaryKey, ErrorUtils.getNPXMessageSupplier("primaryKey"));
        Objects.requireNonNull(entity, ErrorUtils.getNPXMessageSupplier("entity"));
        Objects.requireNonNull(descriptor, ErrorUtils.getNPXMessageSupplier("descriptor"));

        acquireWriteLock();
        try {
            cache.put(primaryKey, entity, descriptor);
        } finally {
            releaseWriteLock();
        }
    }

    /**
     * Releases the live object cache.
     */
    private void releaseCache() {
        acquireWriteLock();
        try {
            this.cache = new TtlCache();
        } finally {
            releaseWriteLock();
        }
    }

    @Override
    public void evictInferredObjects() {
        acquireWriteLock();
        try {
            getInferredClasses().forEach(cache::evict);
        } finally {
            releaseWriteLock();
        }
    }

    @Override
    public <T> T get(Class<T> cls, Object primaryKey, Descriptor descriptor) {
        if (cls == null || primaryKey == null || descriptor == null) {
            return null;
        }
        acquireReadLock();
        try {
            return cache.get(cls, primaryKey, descriptor);
        } finally {
            releaseReadLock();
        }
    }

    /**
     * Get the set of inferred classes.
     * <p>
     * Inferred classes (i. e. classes with inferred attributes) are tracked separately since they require special
     * behavior.
     *
     * @return Set of inferred classes
     */
    public Set<Class<?>> getInferredClasses() {
        if (inferredClasses == null) {
            this.inferredClasses = new HashSet<>();
        }
        return inferredClasses;
    }

    /**
     * Set the inferred classes.
     * <p>
     * For more information about inferred classes see {@link #getInferredClasses()}.
     *
     * @param inferredClasses The set of inferred classes
     */
    @Override
    public void setInferredClasses(Set<Class<?>> inferredClasses) {
        this.inferredClasses = inferredClasses;
    }

    @Override
    public boolean contains(Class<?> cls, Object primaryKey, Descriptor descriptor) {
        if (cls == null || primaryKey == null || descriptor == null) {
            return false;
        }
        acquireReadLock();
        try {
            return cache.contains(cls, primaryKey, descriptor);
        } finally {
            releaseReadLock();
        }
    }

    @Override
    public void evict(Class<?> cls) {
        Objects.requireNonNull(cls);

        acquireWriteLock();
        try {
            cache.evict(cls);
        } finally {
            releaseWriteLock();
        }
    }

    @Override
    public void evict(Class<?> cls, Object identifier, URI context) {
        Objects.requireNonNull(cls, ErrorUtils.getNPXMessageSupplier("cls"));
        Objects.requireNonNull(identifier, ErrorUtils.getNPXMessageSupplier("primaryKey"));

        acquireWriteLock();
        try {
            cache.evict(cls, identifier, context);
        } finally {
            releaseWriteLock();
        }

    }

    @Override
    public void evict(URI context) {
        acquireWriteLock();
        try {
            cache.evict(context);
        } finally {
            releaseWriteLock();
        }
    }

    @Override
    public void evictAll() {
        releaseCache();
    }

    private void acquireReadLock() {
        readLock.lock();
    }

    private void releaseReadLock() {
        readLock.unlock();
    }

    private void acquireWriteLock() {
        writeLock.lock();
    }

    private void releaseWriteLock() {
        writeLock.unlock();
    }

    /**
     * Sweeps the second level cache and removes entities with no more time to live.
     */
    private final class CacheSweeper implements Runnable {

        @Override
        public void run() {
            LOG.trace("Running cache sweep.");
            TtlCacheManager.this.acquireWriteLock();
            try {
                if (TtlCacheManager.this.sweepRunning) {
                    return;
                }
                TtlCacheManager.this.sweepRunning = true;
                final long currentTime = System.currentTimeMillis();
                final List<URI> toEvict = new ArrayList<>();
                // Mark the objects for eviction (can't evict them now, it would
                // cause ConcurrentModificationException)
                for (Entry<URI, Long> e : cache.ttl.entrySet()) {
                    final long lm = e.getValue();
                    if (lm + TtlCacheManager.this.timeToLive < currentTime) {
                        toEvict.add(e.getKey());
                    }
                }
                // Evict them
                toEvict.forEach(TtlCacheManager.this::evict);
            } finally {
                TtlCacheManager.this.sweepRunning = false;
                TtlCacheManager.this.releaseWriteLock();
            }
        }
    }

    private static final class TtlCache extends EntityCache {

        private final Map<URI, Long> ttl = new HashMap<>();

        @Override
        void put(Object identifier, Object entity, Descriptor descriptor) {
            if (!isCacheable(descriptor)) {
                return;
            }
            super.put(identifier, entity, descriptor);
            final URI ctx = descriptor.getSingleContext().orElse(defaultContext);
            updateTimeToLive(ctx);
        }

        @Override
        <T> T get(Class<T> cls, Object identifier, Descriptor descriptor) {
            assert cls != null;
            assert identifier != null;

            return getInternal(cls, identifier, descriptor, this::updateTimeToLive);
        }

        private void updateTimeToLive(URI context) {
            assert context != null;

            ttl.put(context, System.currentTimeMillis());
        }

        @Override
        void evict(URI context) {
            final URI ctx = context != null ? context : defaultContext;
            super.evict(ctx);
            ttl.remove(context);
        }

        @Override
        void evict(Class<?> cls) {
            for (Entry<URI, Map<Object, Map<Class<?>, Object>>> e : repoCache.entrySet()) {
                final Map<Object, Map<Class<?>, Object>> m = e.getValue();
                for (Entry<Object, Map<Class<?>, Object>> indNode : m.entrySet()) {
                    final Object instance = indNode.getValue().remove(cls);
                    if (instance != null) {
                        descriptors.remove(instance);
                    }
                }
                if (m.isEmpty()) {
                    ttl.remove(e.getKey());
                }
            }
        }
    }
}
