package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.sessions.CacheManager;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import static org.junit.Assert.*;

public class CacheManagerTestRunner {

    private final URI CONTEXT_ONE = URI.create("http://jopa-unit-tests");
    private final URI CONTEXT_TWO = URI.create("http://jopa-unit-testsTwo");
    private OWLClassA testA;
    private OWLClassB testB;
    private OWLClassM testM;
    private Map<URI, OWLClassB> listOfBs;

    public CacheManagerTestRunner() {
        final URI pk = URI.create("http://testEntity");
        testA = new OWLClassA();
        testA.setUri(pk);
        testA.setStringAttribute("testAttribute");
        final URI pkB = URI.create("http://testB");
        testB = new OWLClassB();
        testB.setUri(pkB);
        testB.setStringAttribute("stringAttribute");
        testM = new OWLClassM();
        testM.setKey("http://testM");
        listOfBs = new HashMap<>();
        for (int i = 0; i < 10; i++) {
            final URI pkI = URI.create("http://testBList_" + i);
            final OWLClassB b = new OWLClassB();
            b.setUri(pkI);
            b.setStringAttribute("Instance number " + i);
            listOfBs.put(pkI, b);
        }
    }


    public void testAdd(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_TWO);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
        final Object res = manager.get(testA.getClass(), testA.getUri(), CONTEXT_TWO);
        assertNotNull(res);
        assertSame(testA, res);
    }

    public void testAddToDefault(CacheManager manager) {
        manager.add(testA.getUri(), testA, null);
        assertTrue(manager.contains(testA.getClass(), testA.getUri()));
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
        final Object res = manager.get(testA.getClass(), testA.getUri(), null);
        assertNotNull(res);
        assertSame(testA, res);
    }

    public void testAddNull(CacheManager manager) {
        manager.add(URI.create("http://blahblahblah"), null, CONTEXT_TWO);
    }

    public void testAddWithDuplicateIRI(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        final OWLClassA duplicate = new OWLClassA();
        final String newStr = testA.getStringAttribute() + "duplicated";
        duplicate.setStringAttribute(newStr);
        duplicate.setUri(testA.getUri());
        manager.add(duplicate.getUri(), duplicate, CONTEXT_ONE);
        final OWLClassA res = manager.get(testA.getClass(), testA.getUri(), CONTEXT_ONE);
        assertNotNull(res);
        assertFalse(testA.getStringAttribute().equals(res.getStringAttribute()));
        assertEquals(newStr, res.getStringAttribute());
    }

    public void testAddWithDuplicateIRIToDifferentContexts(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_TWO);
        final OWLClassA duplicate = new OWLClassA();
        duplicate.setUri(testA.getUri());
        final String newStr = testA.getStringAttribute() + "duplicated";
        duplicate.setStringAttribute(newStr);
        manager.add(duplicate.getUri(), duplicate, CONTEXT_ONE);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
        assertTrue(manager.contains(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE));
        assertSame(testA, manager.get(testA.getClass(), testA.getUri(), CONTEXT_TWO));
        assertSame(duplicate, manager.get(testA.getClass(), duplicate.getUri(), CONTEXT_ONE));
    }

    public void testContainsDefault(CacheManager manager) {
        manager.add(testA.getUri(), testA, null);
        assertTrue(manager.contains(testA.getClass(), testA.getUri()));
        assertFalse(manager.contains(testB.getClass(), testA.getUri(), CONTEXT_ONE));
    }

    public void testContainsWithContext(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_TWO);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
    }

    public void testContainsNull(CacheManager manager) {
        assertFalse(manager.contains(null, testA.getUri()));
        assertFalse(manager.contains(testA.getClass(), null));
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), null));
        assertFalse(manager.contains(null, testA.getUri(), CONTEXT_TWO));
        assertFalse(manager.contains(testA.getClass(), null, CONTEXT_TWO));
    }

    public void testGetObject(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_TWO);
        final Object res = manager.get(testA.getClass(), testA.getUri(), CONTEXT_TWO);
        assertEquals(testA, res);
    }

    public void testGetObjectWithWrongContext(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_TWO);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
        final OWLClassA res = manager.get(testA.getClass(), testA.getUri(), CONTEXT_ONE);
        assertNull(res);
    }

    public void testGetObjectNull(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_TWO);
        final Object o = manager.get(OWLClassA.class, null, CONTEXT_TWO);
        assertNull(o);
    }

    public void testGetObjectUnknownClass(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_TWO);
        manager.add(testB.getUri(), testB, CONTEXT_TWO);
        final Object o = manager.get(OWLClassD.class, testA.getUri(), CONTEXT_TWO);
        assertNull(o);
    }

    public void testGetObjectUnknownPrimaryKey(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        manager.add(testB.getUri(), testB, CONTEXT_ONE);
        final URI unknownId = URI.create("http://unknownId");
        final Object o = manager.get(OWLClassA.class, unknownId, CONTEXT_ONE);
        assertNull(o);
    }

    public void testEvictAll(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        addAllToCache(listOfBs, manager);
        manager.evictAll();
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        for (OWLClassB b : listOfBs.values()) {
            assertFalse(manager.contains(b.getClass(), b.getUri(), CONTEXT_ONE));
        }
    }

    public Class<?> testEvictByClass(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        manager.add(testB.getUri(), testB, CONTEXT_TWO);
        addAllToCache(listOfBs, manager);
        manager.evict(OWLClassB.class);
        assertTrue(manager.contains(OWLClassA.class, testA.getUri(), CONTEXT_ONE));
        assertFalse(manager.contains(OWLClassB.class, testB.getUri(), CONTEXT_TWO));
        for (OWLClassB b : listOfBs.values()) {
            assertFalse(manager.contains(b.getClass(), b.getUri(), CONTEXT_TWO));
        }
        return OWLClassB.class;
    }

    public void testEvictByClassNull(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        manager.add(testB.getUri(), testB, CONTEXT_TWO);
        addAllToCache(listOfBs, manager);
        manager.evict((Class<?>) null);
    }

    public URI testEvictByContext(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        manager.add(testB.getUri(), testB, CONTEXT_TWO);
        manager.evict(CONTEXT_ONE);
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
        return CONTEXT_ONE;
    }

    public void testEvictByContextNull(CacheManager manager) {
        manager.add(testA.getUri(), testA, null);
        manager.evict((URI) null);
        assertNull(manager.get(testA.getClass(), testA.getUri(), null));
    }

    public void testEvictByContextUnknownContext(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        manager.add(testB.getUri(), testB, CONTEXT_TWO);
        manager.evict(URI.create("http://someUnknownContextUri"));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
    }

    public void testEvictInferredClass(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        manager.add(testB.getUri(), testB, CONTEXT_TWO);
        final Set<Class<?>> inferred = Collections.<Class<?>>singleton(testA.getClass());
        manager.setInferredClasses(inferred);
        manager.clearInferredObjects();
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
    }

    public void testEvictByContextAndPrimaryKey(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_TWO);
        final OWLClassA duplicate = new OWLClassA();
        duplicate.setUri(testA.getUri());
        duplicate.setStringAttribute("Duplicate entity.");
        manager.add(duplicate.getUri(), duplicate, CONTEXT_ONE);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
        assertTrue(manager.contains(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE));

        manager.evict(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE);
        assertFalse(manager.contains(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE));
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
    }

    public void testEvictByContextAndPrimaryKeyNull(CacheManager manager) {
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        manager.evict(null, null, CONTEXT_ONE);
    }

    private void addAllToCache(Map<URI, OWLClassB> entities, CacheManager manager) {
        for (Entry<URI, OWLClassB> e : entities.entrySet()) {
            manager.add(e.getKey(), e.getValue(), CONTEXT_ONE);
        }
    }


    public void cacheAddWithStringIdentifier(CacheManager manager) throws Exception {
        manager.add(testM.getKey(), testM, CONTEXT_ONE);
        assertTrue(manager.contains(OWLClassM.class, testM.getKey(), CONTEXT_ONE));
    }

    public void cacheEvictWithStringIdentifier(CacheManager manager) throws Exception {
        manager.add(testM.getKey(), testM, null);
        assertTrue(manager.contains(OWLClassM.class, testM.getKey()));
        manager.evict(OWLClassM.class, testM.getKey(), null);
        assertFalse(manager.contains(OWLClassM.class, testM.getKey()));
    }
}
