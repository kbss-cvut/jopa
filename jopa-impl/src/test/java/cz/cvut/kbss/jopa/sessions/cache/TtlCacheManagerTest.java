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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;

import static org.junit.jupiter.api.Assertions.*;

public class TtlCacheManagerTest extends AbstractCacheManagerTest<TtlCacheManager> {

    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        this.manager = new TtlCacheManager(Collections.emptyMap());
    }

    @Test
    public void testConstructorNumberInvalid() throws Exception {
        final Map<String, String> m = new HashMap<>();
        m.put(JOPAPersistenceProperties.CACHE_TTL, "1s");
        m.put(JOPAPersistenceProperties.CACHE_SWEEP_RATE, "2");
        final CacheManager man = new TtlCacheManager(m);
        assertNotNull(man);
        final Field ttlField = TtlCacheManager.class.getDeclaredField("timeToLive");
        ttlField.setAccessible(true);
        final Field defaultTtlField = TtlCacheManager.class.getDeclaredField("DEFAULT_TTL");
        defaultTtlField.setAccessible(true);
        final long expected = defaultTtlField.getLong(null);
        final long actual = ttlField.getLong(man);
        assertEquals(expected, actual);
    }

    @Test
    public void testConstructorInvalidSweepRate() throws Exception {
        final Map<String, String> m = new HashMap<>();
        m.put(JOPAPersistenceProperties.CACHE_TTL, "1");
        m.put(JOPAPersistenceProperties.CACHE_SWEEP_RATE, "fdsfa");
        final CacheManager man = new TtlCacheManager(m);
        assertNotNull(man);
        final Field sweepRateField = TtlCacheManager.class.getDeclaredField("sweepRate");
        sweepRateField.setAccessible(true);
        final Field defaultSweepRateField = TtlCacheManager.class
                .getDeclaredField("DEFAULT_SWEEP_RATE");
        defaultSweepRateField.setAccessible(true);
        final long expected = defaultSweepRateField.getLong(null);
        final long actual = sweepRateField.getLong(man);
        assertEquals(expected, actual);
    }

    @Test
    public void testEvictWithSweeper() throws Exception {
        initSweepableManager();
        final Descriptor descriptorOne = descriptor(CONTEXT_ONE);
        final Descriptor descriptorTwo = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptorOne);
        manager.add(testB.getUri(), testB, descriptorTwo);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptorOne));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), descriptorTwo));
        // Give it enough time to sweep the cache
        Thread.sleep(5000);
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), descriptorOne));
        assertFalse(manager.contains(testB.getClass(), testB.getUri(), descriptorTwo));
    }

    @Test
    public void testRefreshTTL() throws Exception {
        initSweepableManager();
        final Descriptor descriptorOne = descriptor(CONTEXT_ONE);
        final Descriptor descriptorTwo = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptorOne);
        manager.add(testB.getUri(), testB, descriptorTwo);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptorOne));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), descriptorTwo));
        // The cycle ensures that testA is refreshed and stays in the cache
        // while testB will be evicted because its TTL is exhausted
        for (int i = 0; i < 8; i++) {
            Thread.sleep(500);
            assertNotNull(manager.get(testA.getClass(), testA.getUri(), descriptorOne));
        }
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptorOne));
        assertFalse(manager.contains(testB.getClass(), testB.getUri(), descriptorTwo));
    }

    private void initSweepableManager() {
        final Map<String, String> props = new HashMap<>();
        props.put(JOPAPersistenceProperties.CACHE_TTL, "1");
        props.put(JOPAPersistenceProperties.CACHE_SWEEP_RATE, "2");
        this.manager = new TtlCacheManager(props);
    }

    @Test
    public void closeShutsDownSweeper() throws Exception {
        initSweepableManager();
        final Field schedulerField = TtlCacheManager.class.getDeclaredField("sweeperScheduler");
        schedulerField.setAccessible(true);
        final ScheduledExecutorService scheduler = (ScheduledExecutorService) schedulerField.get(manager);
        manager.close();
        assertTrue(scheduler.isShutdown());
    }

    @Override
    Map<?, ?> extractDescriptors() throws Exception {
        final Field cacheField = TtlCacheManager.class.getDeclaredField("cache");
        cacheField.setAccessible(true);
        final EntityCache cache = (EntityCache) cacheField.get(manager);
        final Field descriptorsField = EntityCache.class.getDeclaredField("descriptors");
        descriptorsField.setAccessible(true);
        return (Map<?, ?>) descriptorsField.get(cache);
    }
}
