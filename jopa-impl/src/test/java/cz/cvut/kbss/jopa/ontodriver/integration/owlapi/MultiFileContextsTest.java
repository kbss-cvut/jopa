package cz.cvut.kbss.jopa.ontodriver.integration.owlapi;

import static org.junit.Assert.*;

import java.net.URI;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.OWLEntityExistsException;
import cz.cvut.kbss.jopa.ontodriver.TestEnv;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.OWLClassE;
import cz.cvut.kbss.jopa.owlapi.OWLClassI;
import cz.cvut.kbss.jopa.owlapi.utils.StorageInfo;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public class MultiFileContextsTest {

	private static final Logger LOG = Logger.getLogger(MultiFileContextsTest.class.getName());

	private static final List<StorageInfo> storages = initStorages();
	private static final String OWLCLASS_A_REFERENCE_FIELD = "owlClassA";

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;
	// Generated IRI
	private static OWLClassE entityE;
	// Lazy reference to OWLClassA
	private static OWLClassI entityI;

	private static DataSource ds;
	private static PersistenceProviderFacade facade;
	private static Connection c;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		entityA.setTypes(Collections.singleton("JustOneType"));
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://entityD"));
		entityD.setOwlClassA(entityA);
		entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
		entityI = new OWLClassI();
		entityI.setUri(URI.create("http://entityI"));
		entityI.setOwlClassA(entityA);
		facade = TestEnv.getProviderFacade();
	}

	@After
	public void tearDown() throws Exception {
		if (c != null) {
			c.close();
		}
		entityE.setUri(null);
	}

	@Test
	public void testConnect() throws Exception {
		LOG.config("Test: connect to the storages.");
		acquireConnection("MultiFileContextsConnect");
		final List<Context> contexts = c.getContexts();
		assertNotNull(contexts);
		assertEquals(storages.size(), contexts.size());
		// Just search the storages so that we know we are connected
		for (Context ctx : contexts) {
			assertFalse(c.contains(entityA.getUri(), ctx.getUri()));
		}
	}

	@Test
	public void testPersistIntoAll() throws Exception {
		LOG.config("Test: persist into all accessible contexts.");
		acquireConnection("MultiFileContextsPersistIntoAll");
		c.setAutoCommit(false);
		final List<Context> contexts = c.getContexts();
		final URI pk = entityB.getUri();
		for (Context ctx : contexts) {
			c.persist(pk, entityB, ctx.getUri());
		}
		c.commit();
		for (Context ctx : contexts) {
			assertTrue(c.contains(pk, ctx.getUri()));
			final OWLClassB res = c.find(entityB.getClass(), pk, ctx.getUri());
			assertNotNull(res);
			assertEquals(entityB.getStringAttribute(), res.getStringAttribute());
		}
	}

	@Test
	public void testRemoveFromSome() throws Exception {
		LOG.config("Test: persist different entities into contexts and remove one entity from some context.");
		acquireConnection("MultiFileContextsRemoveFromSome");
		c.setAutoCommit(true);
		final List<Context> contexts = c.getContexts();
		assertTrue(contexts.size() > 2);
		final Context ctx = contexts.get(1);
		final Context ctxA = contexts.get(0);
		final Context ctxB = contexts.get(2);
		c.persist(entityA.getUri(), entityA, ctxA.getUri());
		assertNull(entityE.getUri());
		// Persist with ID generation
		c.persist(null, entityE, ctx.getUri());
		assertNotNull(entityE.getUri());
		c.persist(entityB.getUri(), entityB, ctxB.getUri());

		assertTrue(c.contains(entityA.getUri(), ctxA.getUri()));
		assertTrue(c.contains(entityE.getUri(), ctx.getUri()));
		assertTrue(c.contains(entityB.getUri(), ctxB.getUri()));

		final OWLClassE toRemove = c.find(entityE.getClass(), entityE.getUri(), ctx.getUri());
		assertNotNull(toRemove);
		c.remove(toRemove.getUri(), toRemove);
		assertFalse(c.contains(entityE.getUri(), ctx.getUri()));

		assertTrue(c.contains(entityA.getUri(), ctxA.getUri()));
		assertTrue(c.contains(entityB.getUri(), ctxB.getUri()));
	}

	@Test
	public void testMergeInMultiple() throws Exception {
		LOG.config("Test: merge changes in multiple contexts.");
		acquireConnection("MultiFileContextsMerge");
		c.setAutoCommit(false);
		final List<Context> contexts = c.getContexts();
		assertTrue(contexts.size() > 2);
		final Context ctxD = contexts.get(1);
		final Context ctxA = contexts.get(0);
		final Context ctxB = contexts.get(2);
		c.persist(entityA.getUri(), entityA, ctxA.getUri());
		c.persist(entityA.getUri(), entityA, ctxD.getUri());
		c.persist(entityD.getUri(), entityD, ctxD.getUri());
		c.persist(entityB.getUri(), entityB, ctxB.getUri());
		c.commit();

		final OWLClassD toChange = c.find(entityD.getClass(), entityD.getUri());
		assertNotNull(toChange);
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://newAPersisted"));
		newA.setStringAttribute("newAStringAttribute");
		toChange.setOwlClassA(newA);
		c.persist(newA.getUri(), newA, ctxD.getUri());
		c.merge(toChange.getUri(), toChange);
		c.commit();
		assertTrue(c.contains(entityA.getUri(), ctxD.getUri()));
		assertTrue(c.contains(newA.getUri(), ctxD.getUri()));
		assertTrue(c.contains(entityA.getUri(), ctxA.getUri()));
		final OWLClassD res = c.find(entityD.getClass(), entityD.getUri(), ctxD.getUri());
		assertNotNull(res);
		assertEquals(newA.getUri(), res.getOwlClassA().getUri());
	}

	@Test
	public void testLoadField() throws Exception {
		LOG.config("Test: load lazily loaded field value for entity.");
		acquireConnection("MultiFileContextsLoadField");
		c.setAutoCommit(false);
		final List<Context> contexts = c.getContexts();
		assertTrue(contexts.size() > 2);
		final Context ctxI = contexts.get(1);
		final Context ctxA = contexts.get(0);
		final Context ctxB = contexts.get(2);
		c.persist(entityA.getUri(), entityA, ctxA.getUri());
		c.persist(entityA.getUri(), entityA, ctxI.getUri());
		c.persist(entityI.getUri(), entityI, ctxI.getUri());
		c.persist(entityB.getUri(), entityB, ctxB.getUri());
		c.commit();

		final OWLClassI i = c.find(entityI.getClass(), entityI.getUri());
		assertNotNull(i);
		assertNull(i.getOwlClassA());
		c.loadFieldValue(i, i.getClass().getDeclaredField(OWLCLASS_A_REFERENCE_FIELD));
		assertNotNull(i.getOwlClassA());
		assertEquals(entityA.getUri(), i.getOwlClassA().getUri());
		assertEquals(entityA.getStringAttribute(), i.getOwlClassA().getStringAttribute());
	}

	@Test
	public void testSetContextAndPersist() throws Exception {
		LOG.config("Test: set connection context and persist an entity into it.");
		acquireConnection("MultiFileContextsSetContextAndPersist");
		c.setAutoCommit(true);
		final List<Context> contexts = c.getContexts();
		assertTrue(contexts.size() > 2);
		final Context ctx = contexts.get(contexts.size() - 1);
		c.setConnectionContext(ctx.getUri());
		c.persist(entityA.getUri(), entityA);
		for (int i = 0; i < contexts.size() - 1; i++) {
			assertFalse(c.contains(entityA.getUri(), contexts.get(i).getUri()));
		}
		assertTrue(c.contains(entityA.getUri(), ctx.getUri()));
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistTwiceIntoContext() throws Exception {
		LOG.config("Test: try to persist an entity twice into the same context.");
		acquireConnection("MultiFileContextsPersistTwiceIntoContext");
		c.setAutoCommit(true);
		final Context ctx = c.getContexts().get(1);
		c.persist(entityB.getUri(), entityB, ctx.getUri());
		final OWLClassA a = new OWLClassA();
		a.setUri(entityB.getUri());
		a.setStringAttribute("DuplicateEntity");
		c.persist(a.getUri(), a, ctx.getUri());
		fail("This line should not have been reached.");
	}

	@Test
	public void testRollback() throws Exception {
		LOG.config("Test: rollback changes to multiple modules.");
		acquireConnection("MultiFileContextsRollback");
		c.setAutoCommit(false);
		for (Context ctx : c.getContexts()) {
			c.persist(entityA.getUri(), entityA, ctx.getUri());
		}
		c.persist(entityD.getUri(), entityD);
		assertTrue(c.contains(entityD.getUri()));
		c.rollback();
		assertFalse(c.contains(entityD.getUri()));
		for (Context ctx : c.getContexts()) {
			assertNull(c.find(entityA.getClass(), entityA.getUri(), ctx.getUri()));
		}
	}

	private static void acquireConnection(String baseName) throws OntoDriverException {
		assert baseName != null;
		ds = TestEnv.createDataSource(baseName, storages, false);
		c = ds.getConnection(facade);
	}

	private static List<StorageInfo> initStorages() {
		final List<StorageInfo> list = new LinkedList<StorageInfo>();
		list.add(new StorageInfo(OntologyConnectorType.OWLAPI, OwlapiStorageType.FILE));
		list.add(new StorageInfo(OntologyConnectorType.OWLAPI, OwlapiStorageType.FILE));
		list.add(new StorageInfo(OntologyConnectorType.OWLAPI, OwlapiStorageType.FILE));
		return list;
	}
}
