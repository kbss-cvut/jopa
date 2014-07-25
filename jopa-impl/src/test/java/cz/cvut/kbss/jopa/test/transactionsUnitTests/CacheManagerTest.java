package cz.cvut.kbss.jopa.test.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.CacheManagerImpl;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassF;

public class CacheManagerTest {

	private static final Logger LOG = Logger.getLogger(CacheManagerTest.class.getName());

	private static final URI CONTEXT_ONE = URI.create("http://jopa-unit-tests");
	private static final URI CONTEXT_TWO = URI.create("http://jopa-unit-testsTwo");
	private static OWLClassA testA;
	private static OWLClassB testB;
	private static OWLClassE testE;
	private static OWLClassF testF;
	private static Map<URI, OWLClassB> listOfBs;

	private CacheManager mngr;

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
		testE = new OWLClassE();
		final URI pkE = URI.create("http://testE");
		testE.setUri(pkE);
		testE.setStringAttribute("testEStringAttribute");
		testF = new OWLClassF();
		final URI pkF = URI.create("http://testF");
		testF.setUri(pkF);
		listOfBs = new HashMap<URI, OWLClassB>();
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
		this.mngr = new CacheManagerImpl(Collections.<String, String> emptyMap());
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
		mngr.add(testA.getUri(), testA, CONTEXT_TWO);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		assertTrue(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		final Object res = mngr.get(testA.getClass(), testA.getUri(), CONTEXT_TWO);
		assertNotNull(res);
		assertEquals(testA, res);
	}

	@Test
	public void testAddToDefault() {
		LOG.config("Test: add entity into the default context.");
		mngr.add(testA.getUri(), testA, null);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		assertFalse(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertFalse(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		final Object res = mngr.get(testA.getClass(), testA.getUri(), null);
		assertNotNull(res);
		assertSame(testA, res);
	}

	@Test(expected = NullPointerException.class)
	public void testAddNull() {
		LOG.config("Test: add null into cache.");
		mngr.add(URI.create("http://blahblahblah"), null, CONTEXT_TWO);
	}

	@Test
	public void testAddWithDuplicateIRI() {
		LOG.config("Test: add two entities with the same primary key into the same context.");
		mngr.add(testA.getUri(), testA, CONTEXT_ONE);
		final OWLClassA duplicate = new OWLClassA();
		final String newStr = testA.getStringAttribute() + "duplicated";
		duplicate.setStringAttribute(newStr);
		duplicate.setUri(testA.getUri());
		mngr.add(duplicate.getUri(), duplicate, CONTEXT_ONE);
		final OWLClassA res = (OWLClassA) mngr.get(testA.getClass(), testA.getUri(), CONTEXT_ONE);
		assertNotNull(res);
		assertFalse(testA.getStringAttribute().equals(res.getStringAttribute()));
		assertEquals(newStr, res.getStringAttribute());
	}

	@Test
	public void testAddWithDuplicateIRIToDifferentContexts() {
		LOG.config("Test: add two entities with the same primary key into different contexts.");
		mngr.add(testA.getUri(), testA, CONTEXT_TWO);
		final OWLClassA duplicate = new OWLClassA();
		duplicate.setUri(testA.getUri());
		final String newStr = testA.getStringAttribute() + "duplicated";
		duplicate.setStringAttribute(newStr);
		mngr.add(duplicate.getUri(), duplicate, CONTEXT_ONE);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		assertTrue(mngr.contains(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE));
		assertSame(testA, mngr.get(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		assertSame(duplicate, mngr.get(testA.getClass(), duplicate.getUri(), CONTEXT_ONE));
	}

	@Test
	public void testContainsDefault() {
		LOG.config("Test: contains by class and primary key.");
		mngr.add(testA.getUri(), testA, null);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		assertFalse(mngr.contains(testB.getClass(), testA.getUri(), CONTEXT_ONE));
	}

	@Test
	public void testContainsWithContext() {
		LOG.config("Test: contains by entity origin, class and primary key.");
		mngr.add(testA.getUri(), testA, CONTEXT_TWO);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		assertFalse(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
	}

	@Test
	public void testContainsNull() {
		LOG.config("Test: contains, null passed in arguments.");
		assertFalse(mngr.contains(null, testA.getUri()));
		assertFalse(mngr.contains(testA.getClass(), null));
		assertFalse(mngr.contains(testA.getClass(), testA.getUri(), null));
		assertFalse(mngr.contains(null, testA.getUri(), CONTEXT_TWO));
		assertFalse(mngr.contains(testA.getClass(), null, CONTEXT_TWO));
	}

	@Test
	public void testGetObject() {
		LOG.config("Test: get entity.");
		mngr.add(testA.getUri(), testA, CONTEXT_TWO);
		final Object res = mngr.get(testA.getClass(), testA.getUri(), CONTEXT_TWO);
		assertEquals(testA, res);
	}

	@Test
	public void testGetObjectWithWrongContext() {
		LOG.config("Test: get entity. With wrong origin.");
		mngr.add(testA.getUri(), testA, CONTEXT_TWO);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		final OWLClassA res = mngr.get(testA.getClass(), testA.getUri(), CONTEXT_ONE);
		assertNull(res);
	}

	@Test
	public void testGetObjectNull() {
		LOG.config("Test: get entity. Null passed as primary key.");
		mngr.add(testA.getUri(), testA, CONTEXT_TWO);
		final Object o = mngr.get(OWLClassA.class, null, CONTEXT_TWO);
		assertNull(o);
	}

	@Test
	public void testGetObjectUnknownClass() {
		LOG.config("Test: get entity. Unknown class passed.");
		mngr.add(testA.getUri(), testA, CONTEXT_TWO);
		mngr.add(testB.getUri(), testB, CONTEXT_TWO);
		final Object o = mngr.get(OWLClassD.class, testA.getUri(), CONTEXT_TWO);
		assertNull(o);
	}

	@Test
	public void testGetObjectUnknownPrimaryKey() {
		LOG.config("Test: get entity. Unknown primary key.");
		mngr.add(testA.getUri(), testA, CONTEXT_ONE);
		mngr.add(testB.getUri(), testB, CONTEXT_ONE);
		final URI unknownId = URI.create("http://unknownId");
		final Object o = mngr.get(OWLClassA.class, unknownId, CONTEXT_ONE);
		assertNull(o);
	}

	@Test
	public void testEvictAll() {
		LOG.config("Test: evict all.");
		mngr.add(testA.getUri(), testA, CONTEXT_ONE);
		addAllToCache(listOfBs);
		mngr.evictAll();
		assertFalse(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		for (OWLClassB b : listOfBs.values()) {
			assertFalse(mngr.contains(b.getClass(), b.getUri(), CONTEXT_ONE));
		}
	}

	@Test
	public void testEvictByClass() {
		LOG.config("Test: evict class.");
		mngr.add(testA.getUri(), testA, CONTEXT_ONE);
		mngr.add(testB.getUri(), testB, CONTEXT_TWO);
		addAllToCache(listOfBs);
		mngr.evict(OWLClassB.class);
		assertTrue(mngr.contains(OWLClassA.class, testA.getUri(), CONTEXT_ONE));
		assertFalse(mngr.contains(OWLClassB.class, testB.getUri(), CONTEXT_TWO));
		for (OWLClassB b : listOfBs.values()) {
			assertFalse(mngr.contains(b.getClass(), b.getUri(), CONTEXT_TWO));
		}
	}

	@Test(expected = NullPointerException.class)
	public void testEvictByClassNull() {
		LOG.config("Test: evict class. Null passed as class.");
		mngr.add(testA.getUri(), testA, CONTEXT_ONE);
		mngr.add(testB.getUri(), testB, CONTEXT_TWO);
		addAllToCache(listOfBs);
		mngr.evict((Class<?>) null);
	}

	@Test
	public void testEvictByContext() {
		LOG.config("Test: evict by context.");
		mngr.add(testA.getUri(), testA, CONTEXT_ONE);
		mngr.add(testB.getUri(), testB, CONTEXT_TWO);
		mngr.evict(CONTEXT_ONE);
		assertFalse(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertTrue(mngr.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
	}

	@Test
	public void testEvictByContextNull() {
		LOG.config("Test: evict by context. Null passed as context identifier.");
		mngr.add(testA.getUri(), testA, null);
		mngr.evict((URI) null);
		assertNull(mngr.get(testA.getClass(), testA.getUri(), null));
	}

	@Test
	public void testEvictByContextUnknownContext() {
		mngr.add(testA.getUri(), testA, CONTEXT_ONE);
		mngr.add(testB.getUri(), testB, CONTEXT_TWO);
		mngr.evict(URI.create("http://someUnknownContextUri"));
		assertTrue(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertTrue(mngr.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
	}

	@Test
	public void testEvictInferredClass() {
		LOG.config("Set inferred classes and evict them.");
		mngr.add(testA.getUri(), testA, CONTEXT_ONE);
		mngr.add(testB.getUri(), testB, CONTEXT_TWO);
		final Set<Class<?>> inferred = Collections.<Class<?>> singleton(testA.getClass());
		mngr.setInferredClasses(inferred);
		mngr.clearInferredObjects();
		assertFalse(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertTrue(mngr.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
	}

	@Test
	public void testEvictByContextAndPrimaryKey() {
		LOG.config("Test: evict by entity origin, class and primary key.");
		mngr.add(testA.getUri(), testA, CONTEXT_TWO);
		final OWLClassA duplicate = new OWLClassA();
		duplicate.setUri(testA.getUri());
		duplicate.setStringAttribute("Duplicate entity.");
		mngr.add(duplicate.getUri(), duplicate, CONTEXT_ONE);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
		assertTrue(mngr.contains(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE));

		mngr.evict(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE);
		assertFalse(mngr.contains(duplicate.getClass(), duplicate.getUri(), CONTEXT_ONE));
		assertTrue(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_TWO));
	}

	@Test(expected = NullPointerException.class)
	public void testEvictByContextAndPrimaryKeyNull() {
		LOG.config("Test: evict by entity origin, class and primary key. Null passed.");
		mngr.add(testA.getUri(), testA, CONTEXT_ONE);
		mngr.evict(null, null, CONTEXT_ONE);
	}

	@Test
	public void testEvictWithSweeper() throws Exception {
		LOG.config("Test: evict using sweeper.");
		initSweepeableManager();
		mngr.add(testA.getUri(), testA, CONTEXT_ONE);
		mngr.add(testB.getUri(), testB, CONTEXT_TWO);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertTrue(mngr.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
		// Give it enough time to sweep the cache
		Thread.sleep(5000);
		assertFalse(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertFalse(mngr.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
	}

	@Test
	public void testRefreshTTL() throws Exception {
		LOG.config("Test: refresh TTL.");
		initSweepeableManager();
		mngr.add(testA.getUri(), testA, CONTEXT_ONE);
		mngr.add(testB.getUri(), testB, CONTEXT_TWO);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertTrue(mngr.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
		// The cycle ensures that testA is refreshed and stays in the cache
		// while testB will be evicted because its TTL is exhausted
		for (int i = 0; i < 5; i++) {
			Thread.sleep(1000);
			assertNotNull(mngr.get(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		}
		assertTrue(mngr.contains(testA.getClass(), testA.getUri(), CONTEXT_ONE));
		assertFalse(mngr.contains(testB.getClass(), testB.getUri(), CONTEXT_TWO));
	}

	private void initSweepeableManager() {
		final Map<String, String> props = new HashMap<String, String>();
		props.put(OWLAPIPersistenceProperties.CACHE_TTL, "1");
		props.put(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE, "2");
		this.mngr = new CacheManagerImpl(props);
	}

	private void addAllToCache(Map<URI, OWLClassB> entities) {
		for (Entry<URI, OWLClassB> e : entities.entrySet()) {
			mngr.add(e.getKey(), e.getValue(), CONTEXT_ONE);
		}
	}
}
