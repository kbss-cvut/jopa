package cz.cvut.kbss.jopa.owlapi.transactionsUnitTests;

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

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.OWLClassE;
import cz.cvut.kbss.jopa.owlapi.OWLClassF;
import cz.cvut.kbss.jopa.sessions.CacheManagerImpl;
import cz.cvut.kbss.jopa.sessions.ServerSession;

public class CacheManagerTest {

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
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		final Object res = mngr.get(testA.getClass(), testA.getUri());
		assertNotNull(res);
		assertEquals(testA, res);
	}

	@Test(expected = NullPointerException.class)
	public void testAddNull() {
		mngr.add(CONTEXT_URI, URI.create("http://blahblahblah"), null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testAddWithDuplicateIRI() {
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
	public void testGetObject() {
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		Object res = mngr.get(testA.getClass(), testA.getUri());
		assertEquals(testA, res);
	}

	@Test
	public void testGetObjectNull() {
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		try {
			final Object o = mngr.get(OWLClassA.class, null);
			assertNull(o);
		} catch (Exception e) {
			fail("Exception caught. Test failed.");
		}
	}

	@Test
	public void testGetObjectUnknownClass() {
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
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		final URI unknownId = URI.create("http://unknownId");
		final Object o = mngr.get(OWLClassA.class, unknownId);
		assertNull(o);
	}

	@Test
	public void testEvictAll() {
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		addAllToCache(listOfBs);
		mngr.evictAll();
		assertNull(mngr.get(testA.getClass(), testA.getUri()));
		assertTrue(mngr.isEmpty());
	}

	@Test
	public void testEvictByClass() {
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		addAllToCache(listOfBs);
		mngr.evict(OWLClassB.class);
		assertFalse(mngr.isEmpty());
		assertTrue(mngr.contains(OWLClassA.class, testA.getUri()));
		assertFalse(mngr.contains(OWLClassB.class, testB.getUri()));
	}

	@Test
	public void testEvictByClassNull() {
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		addAllToCache(listOfBs);
		try {
			mngr.evict((Class<?>) null);
			assertFalse(mngr.isEmpty());
			assertTrue(mngr.contains(OWLClassA.class, testA.getUri()));
			assertTrue(mngr.contains(OWLClassB.class, testB.getUri()));
		} catch (Exception e) {
			fail("Exception caught. Test failed.");
		}
	}

	@Test
	public void testEvictByPrimaryKey() {
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		final URI pk = URI.create("http://testURI");
		final OWLClassA tmp = new OWLClassA();
		tmp.setUri(pk);
		this.mngr.add(CONTEXT_URI, pk, tmp);
		mngr.evict(OWLClassA.class, pk);
		assertNull(mngr.get(OWLClassA.class, pk));
	}

	@Test
	public void testEvictByPrimaryKeyNull() {
		mngr.add(CONTEXT_URI, testA.getUri(), testA);
		mngr.add(CONTEXT_URI, testB.getUri(), testB);
		try {
			mngr.evict(OWLClassB.class, null);
			assertFalse(mngr.isEmpty());
			assertTrue(mngr.contains(OWLClassA.class, testA.getUri()));
			assertTrue(mngr.contains(OWLClassB.class, testB.getUri()));
		} catch (Exception e) {
			fail("Exception caught. Test failed.");
		}
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
