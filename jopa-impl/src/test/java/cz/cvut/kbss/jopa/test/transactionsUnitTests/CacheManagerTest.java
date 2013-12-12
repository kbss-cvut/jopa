package cz.cvut.kbss.jopa.test.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.CacheManagerImpl;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassF;

public class CacheManagerTest {

	private static final Logger LOG = Logger.getLogger(CacheManagerTest.class.getName());

	private static final URI CONTEXT_URI = URI.create("http://jopa-unit-tests");
	private static SessionStub session;
	private static OWLClassA testA;
	private static OWLClassB testB;
	private static OWLClassE testE;
	private static OWLClassF testF;
	private static Map<URI, OWLClassB> listOfBs;
	private CacheManagerImpl mngr;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		session = new SessionStub();
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
		this.mngr = new CacheManagerImpl(session, Collections.<String, String> emptyMap());
	}

	@Test
	public void testAdd() {
		LOG.config("Test: add entity into cache.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		final Object res = mngr.get(testA.getClass(), testA.getUri());
		assertNotNull(res);
		assertEquals(testA, res);
	}

	@Test(expected = NullPointerException.class)
	public void testAddNull() {
		LOG.config("Test: add null into cache.");
		mngr.add(CONTEXT_URI, URI.create("http://blahblahblah"), null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testAddWithDuplicateIRI() {
		LOG.config("Test: add two entities with the same primary key into the same context.");
		this.mngr.add(CONTEXT_URI, testA.getUri(), testA);
		final OWLClassA duplicate = new OWLClassA();
		final String newStr = testA.getStringAttribute() + "duplicated";
		duplicate.setStringAttribute(newStr);
		duplicate.setUri(testA.getUri());
		mngr.add(CONTEXT_URI, duplicate.getUri(), duplicate);
		final OWLClassA res = (OWLClassA) mngr.get(testA.getClass(), testA.getUri());
		assertNotNull(res);
		assertFalse(newStr.equals(res.getStringAttribute()));
		assertEquals(testA.getStringAttribute(), res.getStringAttribute());
	}

	@Test
	public void testAddWithDuplicateIRIToDifferentContexts() {
		LOG.config("Test: add two entities with the same primary key into different contexts.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		final OWLClassA duplicate = new OWLClassA();
		duplicate.setUri(testA.getUri());
		final String newStr = testA.getStringAttribute() + "duplicated";
		duplicate.setStringAttribute(newStr);
		final URI diffContext = URI.create("http://jopa-different-context");
		mngr.add(diffContext, duplicate.getUri(), duplicate);
		assertTrue(mngr.contains(CONTEXT_URI, testA.getClass(), testA.getUri()));
		assertTrue(mngr.contains(diffContext, duplicate.getClass(), duplicate.getUri()));
	}

	@Test
	public void testContains() {
		LOG.config("Test: contains by class and primary key.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		assertFalse(mngr.contains(testB.getClass(), testA.getUri()));
	}

	@Test
	public void testContainsWithContext() {
		LOG.config("Test: contains by context, class and primary key.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		assertTrue(mngr.contains(CONTEXT_URI, testA.getClass(), testA.getUri()));
		final URI diffContext = URI.create("http://jopa-different-context");
		assertFalse(mngr.contains(diffContext, testA.getClass(), testA.getUri()));
	}

	@Test
	public void testContainsNull() {
		LOG.config("Test: contains, null passed in arguments.");
		try {
			assertFalse(mngr.contains(null, testA.getUri()));
			assertFalse(mngr.contains(testA.getClass(), null));
			assertFalse(mngr.contains(null, testA.getClass(), testA.getUri()));
			assertFalse(mngr.contains(CONTEXT_URI, null, testA.getUri()));
			assertFalse(mngr.contains(CONTEXT_URI, testA.getClass(), null));
		} catch (Exception e) {
			fail("Exception caught. Test failed. Exception: " + e);
		}
	}

	@Test
	public void testGetObject() {
		LOG.config("Test: get entity.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		final Object res = mngr.get(testA.getClass(), testA.getUri());
		assertEquals(testA, res);
	}

	@Test
	public void testGetObjectWithContext() {
		LOG.config("Test: get entity. With context.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		final OWLClassA res = mngr.get(CONTEXT_URI, testA.getClass(), testA.getUri());
		assertNotNull(res);
		final OWLClassB resTwo = mngr.get(CONTEXT_URI, testB.getClass(), testA.getUri());
		assertNull(resTwo);
	}

	@Test
	public void testGetObjectWithWrongContext() {
		LOG.config("Test: get entity. With wrong context.");
		final URI diffContext = URI.create("http://jopa-different-context");
		mngr.add(diffContext, testA.getUri(), testA);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		final OWLClassA res = mngr.get(CONTEXT_URI, testA.getClass(), testA.getUri());
		assertNull(res);
	}

	@Test
	public void testGetObjectNull() {
		LOG.config("Test: get entity. Null passed as primary key.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		try {
			final Object o = mngr.get(OWLClassA.class, null);
			assertNull(o);
		} catch (Exception e) {
			fail("Exception caught. Test failed.");
		}
	}

	@Test
	public void testGetObjectWithContextNull() {
		LOG.config("Test: get entity. Null passed as context URI.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		try {
			final OWLClassA res = mngr.get(null, testA.getClass(), testA.getUri());
			assertNull(res);
		} catch (Exception e) {
			fail("Exception caught. Test failed. Exception : " + e);
		}
	}

	@Test
	public void testGetObjectUnknownClass() {
		LOG.config("Test: get entity. Unknown class passed.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		try {
			final Object o = mngr.get(OWLClassD.class, testA.getUri());
			assertNull(o);
		} catch (Exception e) {
			fail("Exception caught. Test failed.");
		}
	}

	@Test
	public void testGetObjectUnknownPrimaryKey() {
		LOG.config("Test: get entity. Unknown primary key.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		final URI unknownId = URI.create("http://unknownId");
		final Object o = mngr.get(OWLClassA.class, unknownId);
		assertNull(o);
	}

	@Test
	public void testEvictAll() {
		LOG.config("Test: evict all.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		addAllToCache(listOfBs);
		mngr.evictAll();
		assertNull(mngr.get(testA.getClass(), testA.getUri()));
		assertTrue(mngr.isEmpty());
	}

	@Test
	public void testEvictByClass() {
		LOG.config("Test: evict class.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		addAllToCache(listOfBs);
		mngr.evict(OWLClassB.class);
		assertFalse(mngr.isEmpty());
		assertTrue(mngr.contains(OWLClassA.class, testA.getUri()));
		assertFalse(mngr.contains(OWLClassB.class, testB.getUri()));
	}

	@Test(expected = NullPointerException.class)
	public void testEvictByClassNull() {
		LOG.config("Test: evict class. Null passed as class.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		addAllToCache(listOfBs);
		mngr.evict((Class<?>) null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testEvictByPrimaryKey() {
		LOG.config("Test: evict by primary key.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		final URI pk = URI.create("http://testURI");
		final OWLClassA tmp = new OWLClassA();
		tmp.setUri(pk);
		this.mngr.add(CONTEXT_URI, pk, tmp);
		mngr.evict(OWLClassA.class, pk);
		assertNull(mngr.get(OWLClassA.class, pk));
	}

	@Test(expected = NullPointerException.class)
	public void testEvictByPrimaryKeyNull() {
		LOG.config("Test: evict by primary key. Null passed as primary key.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		mngr.evict(OWLClassB.class, null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testEvictByContext() {
		LOG.config("Test: evict by context.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		mngr.evict(CONTEXT_URI);
		assertFalse(mngr.contains(testA.getClass(), testA.getUri()));
		assertFalse(mngr.contains(testB.getClass(), testB.getUri()));
		assertTrue(mngr.isEmpty());
	}

	@Test(expected = NullPointerException.class)
	public void testEvictByContextNull() {
		LOG.config("Test: evict by context. Null passed as context URI.");
		mngr.evict((URI) null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testEvictByContextAndPrimaryKey() {
		LOG.config("Test: evict by context, class and primary key.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		final OWLClassA duplicate = new OWLClassA();
		duplicate.setUri(testA.getUri());
		duplicate.setStringAttribute("Duplicate entity.");
		final URI diffContext = URI.create("http://jopa-different-context");
		mngr.add(diffContext, duplicate.getUri(), duplicate);
		assertTrue(mngr.contains(CONTEXT_URI, testA.getClass(), testA.getUri()));
		assertTrue(mngr.contains(diffContext, duplicate.getClass(), duplicate.getUri()));

		mngr.evict(diffContext, duplicate.getClass(), duplicate.getUri());
		assertFalse(mngr.contains(diffContext, duplicate.getClass(), duplicate.getUri()));
		assertTrue(mngr.contains(CONTEXT_URI, testA.getClass(), testA.getUri()));
	}

	@Test(expected = NullPointerException.class)
	public void testEvictByContextAndPrimaryKeyNull() {
		LOG.config("Test: evict by context, class and primary key. Null passed.");
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.evict(CONTEXT_URI, null, null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testEvictWithSweeper() {
		initSweepeableManager();
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		assertTrue(mngr.contains(testB.getClass(), testB.getUri()));
		// Give it enough time to sweep the cache
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		assertFalse(mngr.contains(testA.getClass(), testA.getUri()));
		assertFalse(mngr.contains(testB.getClass(), testB.getUri()));
	}

	@Test
	public void testRefreshTTL() {
		initSweepeableManager();
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		assertTrue(mngr.contains(testB.getClass(), testB.getUri()));
		// The cycle ensures that testA is refreshed and stays in the cache
		// while testB will be evicted because its TTL is exhausted
		for (int i = 0; i < 5; i++) {
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			assertNotNull(mngr.get(testA.getClass(), testA.getUri()));
		}
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		assertFalse(mngr.contains(testB.getClass(), testB.getUri()));
	}

	private void initSweepeableManager() {
		final Map<String, String> props = new HashMap<String, String>();
		props.put(OWLAPIPersistenceProperties.CACHE_TTL, "1");
		props.put(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE, "2");
		this.mngr = new CacheManagerImpl(session, props);
	}

	private void addAllToCache(Map<URI, OWLClassB> entities) {
		for (Entry<URI, OWLClassB> e : entities.entrySet()) {
			mngr.add(CONTEXT_URI, e.getKey(), e.getValue());
		}
	}

	private static class SessionStub extends ServerSession {

		public SessionStub() {
		}
	}
}
