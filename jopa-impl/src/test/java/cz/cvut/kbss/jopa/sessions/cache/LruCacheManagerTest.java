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
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.Map;

import static org.junit.Assert.*;

public class LruCacheManagerTest {

    private static final URI CONTEXT_ONE = URI.create("http://jopa-unit-tests");
    private static final URI CONTEXT_TWO = URI.create("http://jopa-unit-testsTwo");
    private static OWLClassA testA;
    private static OWLClassB testB;

    private CacheManagerTestRunner testRunner = new CacheManagerTestRunner();

    private LruCacheManager manager;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        testA = new OWLClassA(Generators.createIndividualIdentifier());
        testA.setStringAttribute("testAttribute");
        testB = new OWLClassB(Generators.createIndividualIdentifier());
        testB.setStringAttribute("stringAttribute");
    }

    @Before
    public void setUp() throws Exception {
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

    @Test
    public void testAdd() {
        testRunner.testAdd(manager);
    }

    @Test
    public void testAddToDefault() {
        testRunner.testAddToDefault(manager);
    }

    @Test(expected = NullPointerException.class)
    public void testAddNull() {
        testRunner.testAddNull(manager);
    }

    @Test
    public void testAddWithDuplicateIRI() {
        testRunner.testAddWithDuplicateIRI(manager);
    }

    @Test
    public void testAddWithDuplicateIRIToDifferentContexts() {
        testRunner.testAddWithDuplicateIRIToDifferentContexts(manager);
    }

    @Test
    public void testContainsDefault() {
        testRunner.testContainsDefault(manager);
    }

    @Test
    public void testContainsWithContext() {
        testRunner.testContainsWithContext(manager);
    }

    @Test
    public void testContainsNull() {
        testRunner.testContainsNull(manager);
    }

    @Test
    public void testGetObject() {
        testRunner.testGetObject(manager);
    }

    @Test
    public void testGetObjectWithWrongContext() {
        testRunner.testGetObjectWithWrongContext(manager);
    }

    @Test
    public void testGetObjectNull() {
        testRunner.testGetObjectNull(manager);
    }

    @Test
    public void testGetObjectUnknownClass() {
        testRunner.testGetObjectUnknownClass(manager);
    }

    @Test
    public void testGetObjectUnknownPrimaryKey() {
        testRunner.testGetObjectUnknownPrimaryKey(manager);
    }

    @Test
    public void testEvictAll() {
        testRunner.testEvictAll(manager);
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
        Class<?> evicted = testRunner.testEvictByClass(manager);
        for (LruCache.CacheNode node : getLruCache().keySet()) {
            assertFalse(node.getCls().isAssignableFrom(evicted));
        }
    }

    @Test(expected = NullPointerException.class)
    public void testEvictByClassNull() {
        testRunner.testEvictByClassNull(manager);
    }

    @Test
    public void testEvictByContext() throws Exception {
        URI evicted = testRunner.testEvictByContext(manager);
        for (LruCache.CacheNode node : getLruCache().keySet()) {
            assertNotEquals(evicted, node.getContext());
        }
    }

    @Test
    public void testEvictByContextNull() {
        testRunner.testEvictByContextNull(manager);
    }

    @Test
    public void testEvictByContextUnknownContext() {
        testRunner.testEvictByContextUnknownContext(manager);
    }

    @Test
    public void testEvictInferredClass() {
        testRunner.testEvictInferredClass(manager);
    }

    @Test
    public void testEvictByContextAndPrimaryKey() throws Exception {
        final LruCache lruCache = getLruCache();
        manager.add(testA.getUri(), testA, CONTEXT_TWO);
        final OWLClassA duplicate = new OWLClassA();
        duplicate.setUri(testA.getUri());
        duplicate.setStringAttribute("Duplicate entity.");
        manager.add(duplicate.getUri(), duplicate, CONTEXT_ONE);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
        assertTrue(manager.contains(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE));
        final int size = lruCache.size();

        manager.evict(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE);
        assertFalse(manager.contains(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
        assertEquals(size - 1, lruCache.size());
    }

    @Test(expected = NullPointerException.class)
    public void testEvictByContextAndPrimaryKeyNull() {
        testRunner.testEvictByContextAndPrimaryKeyNull(manager);
    }

    @Test
    public void cacheAddWithStringIdentifier() throws Exception {
        testRunner.cacheAddWithStringIdentifier(manager);
    }

    @Test
    public void cacheEvictWithStringIdentifier() throws Exception {
        testRunner.cacheEvictWithStringIdentifier(manager);
    }

    @Test
    public void entryGetsEvictedWhenCacheIsFull() throws Exception {
        this.manager = new LruCacheManager(
                Collections.singletonMap(JOPAPersistenceProperties.LRU_CACHE_CAPACITY, "2"));
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));

        manager.add(testB.getUri(), testB, CONTEXT_TWO);
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
    }

    @Test
    public void leastRecentlyUsedEntryGetsEvictedWhenCacheIsFull() throws Exception {
        this.manager = new LruCacheManager(
                Collections.singletonMap(JOPAPersistenceProperties.LRU_CACHE_CAPACITY, "4"));
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        manager.add(testB.getUri(), testB, CONTEXT_TWO);
        final OWLClassA aTwo = new OWLClassA(URI.create("http://aTwo"));
        manager.add(aTwo.getUri(), aTwo, CONTEXT_TWO);

        manager.get(testA.getClass(), testA.getUri(), CONTEXT_ONE);
        manager.get(aTwo.getClass(), aTwo.getUri(), CONTEXT_TWO);
        final OWLClassA newA = new OWLClassA(URI.create("http://newA"));
        manager.add(newA.getUri(), newA, null);

        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        assertTrue(manager.contains(aTwo.getClass(), aTwo.getUri(), CONTEXT_TWO));
        assertTrue(manager.contains(newA.getClass(), newA.getUri(), null));
        assertFalse(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
    }
}