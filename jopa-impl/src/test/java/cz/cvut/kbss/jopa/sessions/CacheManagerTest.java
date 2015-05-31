package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ScheduledExecutorService;
import java.util.logging.Logger;

import static org.junit.Assert.*;

public class CacheManagerTest {

	private static final Logger LOG = Logger.getLogger(CacheManagerTest.class.getName());

	private static final URI CONTEXT_ONE = URI.create("http://jopa-unit-tests");
	private static final URI CONTEXT_TWO = URI.create("http://jopa-unit-testsTwo");
	private static OWLClassA testA;
	private static OWLClassB testB;
	private static OWLClassM testM;
	private static Map<URI, OWLClassB> listOfBs;

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

	@Before
	public void setUp() throws Exception {
		this.manager = new CacheManagerImpl(Collections.<String, String> emptyMap());
	}

	@Test
	public void testConstructorNumberInvalid() throws Exception {
		LOG.config("Invalid value for TTL in constructor. Should use default one.");
		final Map<String, String> m = new HashMap<>();
		m.put(OWLAPIPersistenceProperties.CACHE_TTL, "1s");
		m.put(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE, "2");
		final CacheManager man = new CacheManagerImpl(m);
		assertNotNull(man);
		final Field ttlField = CacheManagerImpl.class.getDeclaredField("timeToLive");
		ttlField.setAccessible(true);
		final Field defaultTtlField = CacheManagerImpl.class.getDeclaredField("DEFAULT_TTL");
		defaultTtlField.setAccessible(true);
		final long expected = defaultTtlField.getLong(null);
		final long actual = ttlField.getLong(man);
		assertEquals(expected, actual);
	}

	@Test
	public void testConstructorInvalidSweepRate() throws Exception {
		LOG.config("Invalid value for sweep rate in constructor. Should use default one.");
		final Map<String, String> m = new HashMap<>();
		m.put(OWLAPIPersistenceProperties.CACHE_TTL, "1");
		m.put(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE, "fdsfa");
		final CacheManager man = new CacheManagerImpl(m);
		assertNotNull(man);
		final Field sweepRateField = CacheManagerImpl.class.getDeclaredField("sweepRate");
		sweepRateField.setAccessible(true);
		final Field defaultSweepRateField = CacheManagerImpl.class
				.getDeclaredField("DEFAULT_SWEEP_RATE");
		defaultSweepRateField.setAccessible(true);
		final long expected = defaultSweepRateField.getLong(null);
		final long actual = sweepRateField.getLong(man);
		assertEquals(expected, actual);
	}

	@Test
	public void testAdd() {
		LOG.config("Test: add entity into cache.");
		manager.add(testA.getUri(), testA, CONTEXT_TWO);
		assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		final Object res = manager.get(testA.getClass(), testA.getUri(), CONTEXT_TWO);
		assertNotNull(res);
		assertEquals(testA, res);
	}

	@Test
	public void testAddToDefault() {
		LOG.config("Test: add entity into the default context.");
		manager.add(testA.getUri(), testA, null);
		assertTrue(manager.contains(testA.getClass(), testA.getUri()));
		assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		final Object res = manager.get(testA.getClass(), testA.getUri(), null);
		assertNotNull(res);
		assertSame(testA, res);
	}

	@Test(expected = NullPointerException.class)
	public void testAddNull() {
		LOG.config("Test: add null into cache.");
		manager.add(URI.create("http://blahblahblah"), null, CONTEXT_TWO);
	}

	@Test
	public void testAddWithDuplicateIRI() {
		LOG.config("Test: add two entities with the same primary key into the same context.");
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

	@Test
	public void testAddWithDuplicateIRIToDifferentContexts() {
		LOG.config("Test: add two entities with the same primary key into different contexts.");
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

	@Test
	public void testContainsDefault() {
		LOG.config("Test: contains by class and primary key.");
		manager.add(testA.getUri(), testA, null);
		assertTrue(manager.contains(testA.getClass(), testA.getUri()));
		assertFalse(manager.contains(testB.getClass(), testA.getUri(), CONTEXT_ONE));
	}

	@Test
	public void testContainsWithContext() {
		LOG.config("Test: contains by entity origin, class and primary key.");
		manager.add(testA.getUri(), testA, CONTEXT_TWO);
		assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
	}

	@Test
	public void testContainsNull() {
		LOG.config("Test: contains, null passed in arguments.");
		assertFalse(manager.contains(null, testA.getUri()));
		assertFalse(manager.contains(testA.getClass(), null));
		assertFalse(manager.contains(testA.getClass(), testA.getUri(), null));
		assertFalse(manager.contains(null, testA.getUri(), CONTEXT_TWO));
		assertFalse(manager.contains(testA.getClass(), null, CONTEXT_TWO));
	}

	@Test
	public void testGetObject() {
		LOG.config("Test: get entity.");
		manager.add(testA.getUri(), testA, CONTEXT_TWO);
		final Object res = manager.get(testA.getClass(), testA.getUri(), CONTEXT_TWO);
		assertEquals(testA, res);
	}

	@Test
	public void testGetObjectWithWrongContext() {
		LOG.config("Test: get entity. With wrong origin.");
		manager.add(testA.getUri(), testA, CONTEXT_TWO);
		assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		final OWLClassA res = manager.get(testA.getClass(), testA.getUri(), CONTEXT_ONE);
		assertNull(res);
	}

	@Test
	public void testGetObjectNull() {
		LOG.config("Test: get entity. Null passed as primary key.");
		manager.add(testA.getUri(), testA, CONTEXT_TWO);
		final Object o = manager.get(OWLClassA.class, null, CONTEXT_TWO);
		assertNull(o);
	}

	@Test
	public void testGetObjectUnknownClass() {
		LOG.config("Test: get entity. Unknown class passed.");
		manager.add(testA.getUri(), testA, CONTEXT_TWO);
		manager.add(testB.getUri(), testB, CONTEXT_TWO);
		final Object o = manager.get(OWLClassD.class, testA.getUri(), CONTEXT_TWO);
		assertNull(o);
	}

	@Test
	public void testGetObjectUnknownPrimaryKey() {
		LOG.config("Test: get entity. Unknown primary key.");
		manager.add(testA.getUri(), testA, CONTEXT_ONE);
		manager.add(testB.getUri(), testB, CONTEXT_ONE);
		final URI unknownId = URI.create("http://unknownId");
		final Object o = manager.get(OWLClassA.class, unknownId, CONTEXT_ONE);
		assertNull(o);
	}

	@Test
	public void testEvictAll() {
		LOG.config("Test: evict all.");
		manager.add(testA.getUri(), testA, CONTEXT_ONE);
		addAllToCache(listOfBs);
		manager.evictAll();
		assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		for (OWLClassB b : listOfBs.values()) {
			assertFalse(manager.contains(b.getClass(), b.getUri(), CONTEXT_ONE));
		}
	}

	@Test
	public void testEvictByClass() {
		LOG.config("Test: evict class.");
		manager.add(testA.getUri(), testA, CONTEXT_ONE);
		manager.add(testB.getUri(), testB, CONTEXT_TWO);
		addAllToCache(listOfBs);
		manager.evict(OWLClassB.class);
		assertTrue(manager.contains(OWLClassA.class, testA.getUri(), CONTEXT_ONE));
		assertFalse(manager.contains(OWLClassB.class, testB.getUri(), CONTEXT_TWO));
		for (OWLClassB b : listOfBs.values()) {
			assertFalse(manager.contains(b.getClass(), b.getUri(), CONTEXT_TWO));
		}
	}

	@Test(expected = NullPointerException.class)
	public void testEvictByClassNull() {
		LOG.config("Test: evict class. Null passed as class.");
		manager.add(testA.getUri(), testA, CONTEXT_ONE);
		manager.add(testB.getUri(), testB, CONTEXT_TWO);
		addAllToCache(listOfBs);
		manager.evict((Class<?>) null);
	}

	@Test
	public void testEvictByContext() {
		LOG.config("Test: evict by context.");
		manager.add(testA.getUri(), testA, CONTEXT_ONE);
		manager.add(testB.getUri(), testB, CONTEXT_TWO);
		manager.evict(CONTEXT_ONE);
		assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertTrue(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
	}

	@Test
	public void testEvictByContextNull() {
		LOG.config("Test: evict by context. Null passed as context identifier.");
		manager.add(testA.getUri(), testA, null);
		manager.evict((URI) null);
		assertNull(manager.get(testA.getClass(), testA.getUri(), null));
	}

	@Test
	public void testEvictByContextUnknownContext() {
		manager.add(testA.getUri(), testA, CONTEXT_ONE);
		manager.add(testB.getUri(), testB, CONTEXT_TWO);
		manager.evict(URI.create("http://someUnknownContextUri"));
		assertTrue(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertTrue(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
	}

	@Test
	public void testEvictInferredClass() {
		LOG.config("Set inferred classes and evict them.");
		manager.add(testA.getUri(), testA, CONTEXT_ONE);
		manager.add(testB.getUri(), testB, CONTEXT_TWO);
		final Set<Class<?>> inferred = Collections.<Class<?>> singleton(testA.getClass());
		manager.setInferredClasses(inferred);
		manager.clearInferredObjects();
		assertFalse(manager.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertTrue(manager.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
	}

	@Test
	public void testEvictByContextAndPrimaryKey() {
		LOG.config("Test: evict by entity origin, class and primary key.");
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

	@Test(expected = NullPointerException.class)
	public void testEvictByContextAndPrimaryKeyNull() {
		LOG.config("Test: evict by entity origin, class and primary key. Null passed.");
		manager.add(testA.getUri(), testA, CONTEXT_ONE);
		manager.evict(null, null, CONTEXT_ONE);
	}

	@Test
	public void testEvictWithSweeper() throws Exception {
		LOG.config("Test: evict using sweeper.");
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
		LOG.config("Test: refresh TTL.");
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
		props.put(OWLAPIPersistenceProperties.CACHE_TTL, "1");
		props.put(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE, "2");
		this.manager = new CacheManagerImpl(props);
	}

	private void addAllToCache(Map<URI, OWLClassB> entities) {
		for (Entry<URI, OWLClassB> e : entities.entrySet()) {
			manager.add(e.getKey(), e.getValue(), CONTEXT_ONE);
		}
	}

	@Test
	public void closeShutsDownSweeper() throws Exception {
		initSweepableManager();
		final Field schedulerField = CacheManagerImpl.class.getDeclaredField("sweeperScheduler");
		schedulerField.setAccessible(true);
		final ScheduledExecutorService scheduler = (ScheduledExecutorService) schedulerField.get(manager);
		manager.close();
		assertTrue(scheduler.isShutdown());
	}

	@Test
	public void cacheAddWithStringIdentifier() throws Exception {
		manager.add(testM.getKey(), testM, CONTEXT_ONE);
		assertTrue(manager.contains(OWLClassM.class, testM.getKey(), CONTEXT_ONE));
	}

	@Test
	public void cacheEvictWithStringIdentifier() throws Exception {
		manager.add(testM.getKey(), testM, null);
		assertTrue(manager.contains(OWLClassM.class, testM.getKey()));
		manager.evict(OWLClassM.class, testM.getKey(), null);
		assertFalse(manager.contains(OWLClassM.class, testM.getKey()));
	}
}
