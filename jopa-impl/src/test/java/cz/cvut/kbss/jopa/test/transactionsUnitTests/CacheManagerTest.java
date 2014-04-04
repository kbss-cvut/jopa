package cz.cvut.kbss.jopa.test.transactionsUnitTests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.CacheManagerImpl;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassF;

public class CacheManagerTest {

	private static final Logger LOG = Logger.getLogger(CacheManagerTest.class.getName());

	private static List<Repository> repositories;
	private static final URI CONTEXT_URI = URI.create("http://jopa-unit-tests");
	private static final URI CONTEXT_TWO = URI.create("http://jopa-unit-testsTwo");
	private static ServerSession session;
	private static OWLClassA testA;
	private static OWLClassB testB;
	private static OWLClassE testE;
	private static OWLClassF testF;
	private static Map<URI, OWLClassB> listOfBs;

	private static RepositoryID repoOneCtxOne;
	private static RepositoryID repoTwoCtxOne;
	private static RepositoryID repoTwoCtxTwo;

	private CacheManager mngr;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		initRepositories();
		repoOneCtxOne = RepositoryID.repository(repositories.get(0))
				.contexts(repositories.get(0).getContexts()).build();
		repoTwoCtxOne = RepositoryID.repository(repositories.get(1)).context(CONTEXT_URI).build();
		repoTwoCtxTwo = RepositoryID.repository(repositories.get(1)).context(CONTEXT_TWO).build();
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

	@Test(expected = OWLPersistenceException.class)
	public void testConstructorNumberInvalid() {
		LOG.config("Invalid value for TTL in constructor.");
		final Map<String, String> m = new HashMap<>();
		m.put(OWLAPIPersistenceProperties.CACHE_TTL, "1s");
		m.put(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE, "2");
		final CacheManager man = new CacheManagerImpl(session, m);
		assertNull(man);
	}

	@Test
	public void testAdd() {
		LOG.config("Test: add entity into cache.");
		mngr.add(repoOneCtxOne, testA.getUri(), testA);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		assertTrue(mngr.contains(repoOneCtxOne, testA.getClass(), testA.getUri()));
		final Object res = mngr.get(repoOneCtxOne, testA.getClass(), testA.getUri());
		assertNotNull(res);
		assertEquals(testA, res);
	}

	@Test(expected = NullPointerException.class)
	public void testAddNull() {
		LOG.config("Test: add null into cache.");
		mngr.add(repoOneCtxOne, URI.create("http://blahblahblah"), null);
	}

	@Test
	public void testAddWithDuplicateIRI() {
		LOG.config("Test: add two entities with the same primary key into the same context.");
		mngr.add(repoOneCtxOne, testA.getUri(), testA);
		final OWLClassA duplicate = new OWLClassA();
		final String newStr = testA.getStringAttribute() + "duplicated";
		duplicate.setStringAttribute(newStr);
		duplicate.setUri(testA.getUri());
		mngr.add(repoOneCtxOne, duplicate.getUri(), duplicate);
		final OWLClassA res = (OWLClassA) mngr.get(repoOneCtxOne, testA.getClass(), testA.getUri());
		assertNotNull(res);
		assertFalse(testA.getStringAttribute().equals(res.getStringAttribute()));
		assertEquals(newStr, res.getStringAttribute());
	}

	@Test
	public void testAddWithDuplicateIRIToDifferentContexts() {
		LOG.config("Test: add two entities with the same primary key into different contexts.");
		mngr.add(repoTwoCtxOne, testA.getUri(), testA);
		final OWLClassA duplicate = new OWLClassA();
		duplicate.setUri(testA.getUri());
		final String newStr = testA.getStringAttribute() + "duplicated";
		duplicate.setStringAttribute(newStr);
		mngr.add(repoTwoCtxTwo, duplicate.getUri(), duplicate);
		assertTrue(mngr.contains(repoTwoCtxOne, testA.getClass(), testA.getUri()));
		assertTrue(mngr.contains(repoTwoCtxTwo, duplicate.getClass(), duplicate.getUri()));
		assertSame(testA, mngr.get(repoTwoCtxOne, testA.getClass(), testA.getUri()));
		assertSame(duplicate, mngr.get(repoTwoCtxTwo, testA.getClass(), duplicate.getUri()));
	}

	@Test
	public void testContains() {
		LOG.config("Test: contains by class and primary key.");
		mngr.add(repoOneCtxOne, testA.getUri(), testA);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		assertFalse(mngr.contains(testB.getClass(), testA.getUri()));
	}

	@Test
	public void testContainsWithRepoId() {
		LOG.config("Test: contains by repository id, class and primary key.");
		mngr.add(repoTwoCtxOne, testA.getUri(), testA);
		assertTrue(mngr.contains(repoTwoCtxOne, testA.getClass(), testA.getUri()));
		assertFalse(mngr.contains(repoTwoCtxTwo, testA.getClass(), testA.getUri()));
	}

	@Test
	public void testContainsNull() {
		LOG.config("Test: contains, null passed in arguments.");
		assertFalse(mngr.contains(null, testA.getUri()));
		assertFalse(mngr.contains(testA.getClass(), null));
		assertFalse(mngr.contains(null, testA.getClass(), testA.getUri()));
		assertFalse(mngr.contains(repoOneCtxOne, null, testA.getUri()));
		assertFalse(mngr.contains(repoTwoCtxOne, testA.getClass(), null));
	}

	@Test
	public void testGetObject() {
		LOG.config("Test: get entity.");
		mngr.add(repoTwoCtxTwo, testA.getUri(), testA);
		final Object res = mngr.get(repoTwoCtxTwo, testA.getClass(), testA.getUri());
		assertEquals(testA, res);
	}

	@Test
	public void testGetObjectWithWrongContext() {
		LOG.config("Test: get entity. With wrong context.");
		mngr.add(repoTwoCtxOne, testA.getUri(), testA);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		final OWLClassA res = mngr.get(repoTwoCtxTwo, testA.getClass(), testA.getUri());
		assertNull(res);
	}

	@Test
	public void testGetObjectNull() {
		LOG.config("Test: get entity. Null passed as primary key.");
		mngr.add(repoOneCtxOne, testA.getUri(), testA);
		final Object o = mngr.get(repoOneCtxOne, OWLClassA.class, null);
		assertNull(o);
	}

	@Test
	public void testGetObjectWithContextNull() {
		LOG.config("Test: get entity. Null passed as context URI.");
		mngr.add(repoTwoCtxOne, testA.getUri(), testA);
		final OWLClassA res = mngr.get(null, testA.getClass(), testA.getUri());
		assertNull(res);
	}

	@Test
	public void testGetObjectUnknownClass() {
		LOG.config("Test: get entity. Unknown class passed.");
		mngr.add(repoOneCtxOne, testA.getUri(), testA);
		mngr.add(repoOneCtxOne, testB.getUri(), testB);
		final Object o = mngr.get(repoOneCtxOne, OWLClassD.class, testA.getUri());
		assertNull(o);
	}

	@Test
	public void testGetObjectUnknownPrimaryKey() {
		LOG.config("Test: get entity. Unknown primary key.");
		mngr.add(repoTwoCtxTwo, testA.getUri(), testA);
		mngr.add(repoTwoCtxTwo, testB.getUri(), testB);
		final URI unknownId = URI.create("http://unknownId");
		final Object o = mngr.get(repoTwoCtxTwo, OWLClassA.class, unknownId);
		assertNull(o);
	}

	@Test
	public void testEvictAll() {
		LOG.config("Test: evict all.");
		mngr.add(repoOneCtxOne, testA.getUri(), testA);
		addAllToCache(listOfBs);
		mngr.evictAll();
		assertFalse(mngr.contains(testA.getClass(), testA.getUri()));
		for (OWLClassB b : listOfBs.values()) {
			assertFalse(mngr.contains(b.getClass(), b.getUri()));
		}
	}

	@Test
	public void testEvictByClass() {
		LOG.config("Test: evict class.");
		mngr.add(repoOneCtxOne, testA.getUri(), testA);
		mngr.add(repoTwoCtxOne, testB.getUri(), testB);
		addAllToCache(listOfBs);
		mngr.evict(OWLClassB.class);
		assertTrue(mngr.contains(OWLClassA.class, testA.getUri()));
		assertFalse(mngr.contains(OWLClassB.class, testB.getUri()));
		for (OWLClassB b : listOfBs.values()) {
			assertFalse(mngr.contains(b.getClass(), b.getUri()));
		}
	}

	@Test(expected = NullPointerException.class)
	public void testEvictByClassNull() {
		LOG.config("Test: evict class. Null passed as class.");
		mngr.add(repoOneCtxOne, testA.getUri(), testA);
		mngr.add(repoTwoCtxOne, testB.getUri(), testB);
		addAllToCache(listOfBs);
		mngr.evict((Class<?>) null);
	}

	@Test
	public void testEvictByContext() {
		LOG.config("Test: evict by context.");
		mngr.add(repoTwoCtxOne, testA.getUri(), testA);
		mngr.add(repoTwoCtxTwo, testB.getUri(), testB);
		mngr.evict(repoTwoCtxOne);
		assertFalse(mngr.contains(testA.getClass(), testA.getUri()));
		assertTrue(mngr.contains(testB.getClass(), testB.getUri()));
	}

	@Test(expected = NullPointerException.class)
	public void testEvictByContextNull() {
		LOG.config("Test: evict by context. Null passed as context URI.");
		mngr.evict((RepositoryID) null);
	}

	@Test
	public void testEvictInferredClass() {
		LOG.config("Set inferred classes and evict them.");
		mngr.add(repoTwoCtxOne, testA.getUri(), testA);
		mngr.add(repoTwoCtxTwo, testB.getUri(), testB);
		final Set<Class<?>> inferred = Collections.<Class<?>> singleton(testA.getClass());
		mngr.setInferredClasses(inferred);
		mngr.clearInferredObjects();
		assertFalse(mngr.contains(testA.getClass(), testA.getUri()));
		assertTrue(mngr.contains(testB.getClass(), testB.getUri()));
	}

	@Test
	public void testEvictByContextAndPrimaryKey() {
		LOG.config("Test: evict by context, class and primary key.");
		mngr.add(repoTwoCtxTwo, testA.getUri(), testA);
		final OWLClassA duplicate = new OWLClassA();
		duplicate.setUri(testA.getUri());
		duplicate.setStringAttribute("Duplicate entity.");
		mngr.add(repoTwoCtxOne, duplicate.getUri(), duplicate);
		assertTrue(mngr.contains(repoTwoCtxTwo, testA.getClass(), testA.getUri()));
		assertTrue(mngr.contains(repoTwoCtxOne, duplicate.getClass(), duplicate.getUri()));

		mngr.evict(repoTwoCtxOne, duplicate.getClass(), duplicate.getUri());
		assertFalse(mngr.contains(repoTwoCtxOne, duplicate.getClass(), duplicate.getUri()));
		assertTrue(mngr.contains(repoTwoCtxTwo, testA.getClass(), testA.getUri()));
	}

	@Test(expected = NullPointerException.class)
	public void testEvictByContextAndPrimaryKeyNull() {
		LOG.config("Test: evict by context, class and primary key. Null passed.");
		mngr.add(repoOneCtxOne, testA.getUri(), testA);
		mngr.evict(repoOneCtxOne, null, null);
	}

	@Test
	public void testEvictWithSweeper() throws Exception {
		initSweepeableManager();
		mngr.add(repoOneCtxOne, testA.getUri(), testA);
		mngr.add(repoTwoCtxOne, testB.getUri(), testB);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		assertTrue(mngr.contains(testB.getClass(), testB.getUri()));
		// Give it enough time to sweep the cache
		Thread.sleep(5000);
		assertFalse(mngr.contains(testA.getClass(), testA.getUri()));
		assertFalse(mngr.contains(testB.getClass(), testB.getUri()));
	}

	@Test
	public void testRefreshTTL() throws Exception {
		initSweepeableManager();
		mngr.add(repoTwoCtxOne, testA.getUri(), testA);
		mngr.add(repoOneCtxOne, testB.getUri(), testB);
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		assertTrue(mngr.contains(testB.getClass(), testB.getUri()));
		// The cycle ensures that testA is refreshed and stays in the cache
		// while testB will be evicted because its TTL is exhausted
		for (int i = 0; i < 5; i++) {
			Thread.sleep(1000);
			assertNotNull(mngr.get(repoTwoCtxOne, testA.getClass(), testA.getUri()));
		}
		assertTrue(mngr.contains(testA.getClass(), testA.getUri()));
		assertFalse(mngr.contains(testB.getClass(), testB.getUri()));
	}

	@Test
	public void testReadLock() throws Exception {
		boolean res = mngr.acquireReadLock();
		assertTrue(res);
		Thread t = new Thread(new Runnable() {
			@Override
			public void run() {
				assertTrue(mngr.acquireReadLock());
				mngr.releaseReadLock();
			}
		});
		t.start();
		t.join();
		mngr.releaseReadLock();
		assertTrue(mngr.acquireWriteLock());
	}

	private void initSweepeableManager() {
		final Map<String, String> props = new HashMap<String, String>();
		props.put(OWLAPIPersistenceProperties.CACHE_TTL, "1");
		props.put(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE, "2");
		this.mngr = new CacheManagerImpl(session, props);
	}

	private void addAllToCache(Map<URI, OWLClassB> entities) {
		for (Entry<URI, OWLClassB> e : entities.entrySet()) {
			mngr.add(repoOneCtxOne, e.getKey(), e.getValue());
		}
	}

	private static void initRepositories() {
		repositories = new ArrayList<>(2);
		final Repository rOne = new Repository(URI.create("http://localhost:8080/repoOne"));
		rOne.addContext(CONTEXT_URI);
		repositories.add(rOne);
		final Repository rTwo = new Repository(URI.create("http://localhost:8080/repoTwo"));
		rTwo.addContext(CONTEXT_URI);
		rTwo.addContext(CONTEXT_TWO);
		repositories.add(rTwo);
	}

	private static class SessionStub extends ServerSession {

		public SessionStub() {
		}

		@Override
		public List<Repository> getRepositories() {
			return repositories;
		}
	}
}
