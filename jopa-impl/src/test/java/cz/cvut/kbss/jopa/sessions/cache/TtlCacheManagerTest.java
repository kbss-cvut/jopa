package cz.cvut.kbss.jopa.sessions.cache;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;

import static org.junit.Assert.*;

/**
 * @author kidney
 */
public class TtlCacheManagerTest {

    private static final URI CONTEXT_ONE = URI.create("http://jopa-unit-tests");
    private static final URI CONTEXT_TWO = URI.create("http://jopa-unit-testsTwo");
    private static OWLClassA testA;
    private static OWLClassB testB;

    private CacheManagerTestRunner testRunner = new CacheManagerTestRunner();

    private CacheManager manager;

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

    @Test
    public void testEvictByClass() {
        testRunner.testEvictByClass(manager);
    }

    @Test(expected = NullPointerException.class)
    public void testEvictByClassNull() {
        testRunner.testEvictByClassNull(manager);
    }

    @Test
    public void testEvictByContext() {
        testRunner.testEvictByContext(manager);
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
    public void testEvictByContextAndPrimaryKey() {
        testRunner.testEvictByContextAndPrimaryKey(manager);
    }

    @Test(expected = NullPointerException.class)
    public void testEvictByContextAndPrimaryKeyNull() {
        testRunner.testEvictByContextAndPrimaryKeyNull(manager);
    }

    @Test
    public void testEvictWithSweeper() throws Exception {
        initSweepableManager();
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        manager.add(testB.getUri(), testB, CONTEXT_TWO);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
        // Give it enough time to sweep the cache
        Thread.sleep(5000);
        assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        assertFalse(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
    }

    @Test
    public void testRefreshTTL() throws Exception {
        initSweepableManager();
        manager.add(testA.getUri(), testA, CONTEXT_ONE);
        manager.add(testB.getUri(), testB, CONTEXT_TWO);
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        assertTrue(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
        // The cycle ensures that testA is refreshed and stays in the cache
        // while testB will be evicted because its TTL is exhausted
        for (int i = 0; i < 5; i++) {
            Thread.sleep(1000);
            assertNotNull(manager.get(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        }
        assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
        assertFalse(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
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

    @Test
    public void cacheAddWithStringIdentifier() throws Exception {
        testRunner.cacheAddWithStringIdentifier(manager);
    }

    @Test
    public void cacheEvictWithStringIdentifier() throws Exception {
        testRunner.cacheEvictWithStringIdentifier(manager);
    }
}
