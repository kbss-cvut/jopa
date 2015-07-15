package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.internal.matchers.Null;

import java.net.URI;
import java.util.Collections;
import java.util.Map;

import static org.junit.Assert.assertEquals;

/**
 * @author kidney
 */
public class LruCacheManagerTest {

    private static final URI CONTEXT_ONE = URI.create("http://jopa-unit-tests");
    private static final URI CONTEXT_TWO = URI.create("http://jopa-unit-testsTwo");
    private static OWLClassA testA;
    private static OWLClassB testB;

    private CacheManagerTestRunner testRunner = new CacheManagerTestRunner();

    private LruCacheManager manager;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        final URI pk = URI.create("http://testEntity");
        testA = new OWLClassA();
        testA.setUri(pk);
        testA.setStringAttribute("testAttribute");
        final URI pkB = URI.create("http://testB");
        testB = new OWLClassB();
        testB.setUri(pkB);
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
        final Map<String, String> props = Collections.singletonMap(OWLAPIPersistenceProperties.LRU_CACHE_CAPACITY, Integer.toString(capacity));
        this.manager = new LruCacheManager(props);
        assertEquals(capacity, manager.getCapacity());
    }

    @Test
    public void initializationWithInvalidCapacityUsesDefaultValue() {
        final Map<String, String> props = Collections.singletonMap(OWLAPIPersistenceProperties.LRU_CACHE_CAPACITY, "-111");
        this.manager = new LruCacheManager(props);
        assertEquals(LruCacheManager.DEFAULT_CAPACITY, manager.getCapacity());
    }

    @Test(expected = NullPointerException.class)
    public void npxForNullConstructorArgument() {
        this.manager = new LruCacheManager(null);
    }

    @Test
    public void testAdd() {
        testRunner.testAdd(manager);
    }

//    @Test
//    public void testAddToDefault() {
//        testRunner.testAddToDefault(manager);
//    }
//
//    @Test(expected = NullPointerException.class)
//    public void testAddNull() {
//        testRunner.testAddNull(manager);
//    }
//
//    @Test
//    public void testAddWithDuplicateIRI() {
//        testRunner.testAddWithDuplicateIRI(manager);
//    }
//
//    @Test
//    public void testAddWithDuplicateIRIToDifferentContexts() {
//        testRunner.testAddWithDuplicateIRIToDifferentContexts(manager);
//    }
//
//    @Test
//    public void testContainsDefault() {
//        testRunner.testContainsDefault(manager);
//    }
//
//    @Test
//    public void testContainsWithContext() {
//        testRunner.testContainsWithContext(manager);
//    }
//
//    @Test
//    public void testContainsNull() {
//        testRunner.testContainsNull(manager);
//    }
//
//    @Test
//    public void testGetObject() {
//        testRunner.testGetObject(manager);
//    }
//
//    @Test
//    public void testGetObjectWithWrongContext() {
//        testRunner.testGetObjectWithWrongContext(manager);
//    }
//
//    @Test
//    public void testGetObjectNull() {
//        testRunner.testGetObjectNull(manager);
//    }
//
//    @Test
//    public void testGetObjectUnknownClass() {
//        testRunner.testGetObjectUnknownClass(manager);
//    }
//
//    @Test
//    public void testGetObjectUnknownPrimaryKey() {
//        testRunner.testGetObjectUnknownPrimaryKey(manager);
//    }
//
//    @Test
//    public void testEvictAll() {
//        testRunner.testEvictAll(manager);
//    }
//
//    @Test
//    public void testEvictByClass() {
//        testRunner.testEvictByClass(manager);
//    }
//
//    @Test(expected = NullPointerException.class)
//    public void testEvictByClassNull() {
//        testRunner.testEvictByClassNull(manager);
//    }
//
//    @Test
//    public void testEvictByContext() {
//        testRunner.testEvictByContext(manager);
//    }
//
//    @Test
//    public void testEvictByContextNull() {
//        testRunner.testEvictByContextNull(manager);
//    }
//
//    @Test
//    public void testEvictByContextUnknownContext() {
//        testRunner.testEvictByContextUnknownContext(manager);
//    }
//
//    @Test
//    public void testEvictInferredClass() {
//        testRunner.testEvictInferredClass(manager);
//    }
//
//    @Test
//    public void testEvictByContextAndPrimaryKey() {
//        testRunner.testEvictByContextAndPrimaryKey(manager);
//    }
//
//    @Test(expected = NullPointerException.class)
//    public void testEvictByContextAndPrimaryKeyNull() {
//        testRunner.testEvictByContextAndPrimaryKeyNull(manager);
//    }
//
//    @Test
//    public void cacheAddWithStringIdentifier() throws Exception {
//        testRunner.cacheAddWithStringIdentifier(manager);
//    }
//
//    @Test
//    public void cacheEvictWithStringIdentifier() throws Exception {
//        testRunner.cacheEvictWithStringIdentifier(manager);
//    }
}