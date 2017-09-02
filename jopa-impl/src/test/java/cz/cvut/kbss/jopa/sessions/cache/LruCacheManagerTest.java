/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.Map;

import static org.junit.Assert.*;

public class LruCacheManagerTest extends AbstractCacheManagerTest<LruCacheManager> {

    @Before
    public void setUp() throws Exception {
        super.setUp();
        this.manager = new LruCacheManager();
    }

    @Test
    public void testInitWithDefaultCapacity() {
        assertEquals(LruCacheManager.DEFAULT_CAPACITY, manager.getCapacity());
    }

    @Test
    public void testInitWithCustomCapacity() {
        int capacity = 117;
        final Map<String, String> props = Collections
                .singletonMap(JOPAPersistenceProperties.LRU_CACHE_CAPACITY, Integer.toString(capacity));
        this.manager = new LruCacheManager(props);
        assertEquals(capacity, manager.getCapacity());
    }

    @Test
    public void initializationWithInvalidCapacityUsesDefaultValue() {
        final Map<String, String> props = Collections
                .singletonMap(JOPAPersistenceProperties.LRU_CACHE_CAPACITY, "-111");
        this.manager = new LruCacheManager(props);
        assertEquals(LruCacheManager.DEFAULT_CAPACITY, manager.getCapacity());
    }

    private LruCache getLruCache() throws Exception {
        final Field entityCacheField = LruCacheManager.class.getDeclaredField("entityCache");
        entityCacheField.setAccessible(true);
        final LruCacheManager.LruEntityCache entityCache = (LruCacheManager.LruEntityCache) entityCacheField
                .get(manager);
        final Field lruCacheField = LruCacheManager.LruEntityCache.class.getDeclaredField("cache");
        lruCacheField.setAccessible(true);
        return (LruCache) lruCacheField.get(entityCache);
    }

    @Test
    public void testEvictByClass() throws Exception {
        Class<?> evicted = evictByClass();
        for (LruCache.CacheNode node : getLruCache().keySet()) {
            assertFalse(node.getCls().isAssignableFrom(evicted));
        }
    }

    @Test
    public void testEvictByContext() throws Exception {
        URI evicted = evictByContext();
        for (LruCache.CacheNode node : getLruCache().keySet()) {
            assertNotEquals(evicted, node.getContext());
        }
    }

    @Test
    public void testEvictByContextAndPrimaryKey() throws Exception {
        final LruCache lruCache = getLruCache();
        final Descriptor descriptorOne = descriptor(CONTEXT_ONE);
        final Descriptor descriptorTwo = descriptor(CONTEXT_TWO);
        manager.add(testA.getUri(), testA, descriptorTwo);
        final OWLClassA duplicate = new OWLClassA();
        duplicate.setUri(testA.getUri());
        duplicate.setStringAttribute("Duplicate entity.");
        manager.add(duplicate.getUri(), duplicate, descriptorOne);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptorTwo));
        assertTrue(manager.contains(duplicate.getClass(), duplicate.getUri(), descriptorOne));
        final int size = lruCache.size();

        manager.evict(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE);
        assertFalse(manager.contains(duplicate.getClass(), duplicate.getUri(), descriptorOne));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptorTwo));
        assertEquals(size - 1, lruCache.size());
    }

    @Test
    public void entryGetsEvictedWhenCacheIsFull() throws Exception {
        final Descriptor descriptorOne = descriptor(CONTEXT_ONE);
        final Descriptor descriptorTwo = descriptor(CONTEXT_TWO);
        this.manager = new LruCacheManager(
                Collections.singletonMap(JOPAPersistenceProperties.LRU_CACHE_CAPACITY, "2"));
        manager.add(testA.getUri(), testA, descriptorOne);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptorOne));

        manager.add(testB.getUri(), testB, descriptorTwo);
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), descriptorOne));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), descriptorTwo));
    }

    @Test
    public void leastRecentlyUsedEntryGetsEvictedWhenCacheIsFull() throws Exception {
        final Descriptor descriptorOne = descriptor(CONTEXT_ONE);
        final Descriptor descriptorTwo = descriptor(CONTEXT_TWO);
        this.manager = new LruCacheManager(
                Collections.singletonMap(JOPAPersistenceProperties.LRU_CACHE_CAPACITY, "4"));
        manager.add(testA.getUri(), testA, descriptorOne);
        manager.add(testB.getUri(), testB, descriptorTwo);
        final OWLClassA aTwo = new OWLClassA(URI.create("http://aTwo"));
        manager.add(aTwo.getUri(), aTwo, descriptorTwo);

        manager.get(testA.getClass(), testA.getUri(), descriptorOne);
        manager.get(aTwo.getClass(), aTwo.getUri(), descriptorTwo);
        final OWLClassA newA = new OWLClassA(URI.create("http://newA"));
        manager.add(newA.getUri(), newA, descriptor(null));

        assertTrue(manager.contains(testA.getClass(), testA.getUri(), descriptorOne));
        assertTrue(manager.contains(aTwo.getClass(), aTwo.getUri(), descriptorTwo));
        assertTrue(manager.contains(newA.getClass(), newA.getUri(), descriptor(null)));
        assertFalse(manager.contains(testB.getClass(), testB.getUri(), descriptorTwo));
    }

    @Override
    Map<?, ?> extractDescriptors() throws Exception {
        final Field cacheField = LruCacheManager.class.getDeclaredField("entityCache");
        cacheField.setAccessible(true);
        final EntityCache cache = (EntityCache) cacheField.get(manager);
        final Field descriptorsField = EntityCache.class.getDeclaredField("descriptors");
        descriptorsField.setAccessible(true);
        return (Map<?, ?>) descriptorsField.get(cache);
    }
}