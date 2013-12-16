package cz.cvut.kbss.ontodriver.test.owlapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.utils.JenaStorageConfig;
import cz.cvut.kbss.jopa.test.utils.OwlapiStorageConfig;
import cz.cvut.kbss.jopa.test.utils.OwldbStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.test.TestEnv;

public class MixedMultiContextsTest {

	private static final Logger LOG = Logger.getLogger(MixedMultiContextsTest.class.getName());

	private static final List<StorageConfig> storages = initStorages();
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
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		entityA.setTypes(Collections.singleton("JustOneType"));
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
		entityD.setOwlClassA(entityA);
		entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
		entityI = new OWLClassI();
		entityI.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityI"));
		entityI.setOwlClassA(entityA);
		TestEnvironment.resetOwldbHibernateProvider();
		facade = TestEnv.getProviderFacade();
	}

	@Before
	public void setUp() throws Exception {
		TestEnvironment.clearDatabase();
	}

	@After
	public void tearDown() throws Exception {
		if (c != null) {
			c.close();
		}
		entityE.setUri(null);
	}

	@Test
	public void testPersistIntoAll() throws Exception {
		LOG.config("Test: persist the same entity into all contexts.");
		acquireConnection("MixedMultiContextsPersistIntoAll");
		c.setAutoCommit(false);
		final List<Context> contexts = c.getContexts();
		final URI uriB = entityB.getUri();
		for (Context ctx : contexts) {
			c.persist(uriB, entityB, ctx.getUri());
		}
		c.commit();
		for (Context ctx : contexts) {
			assertTrue(c.contains(uriB, ctx.getUri()));
			assertNotNull(c.find(entityB.getClass(), uriB, ctx.getUri()));
		}
	}

	@Test
	public void testMergeInSome() throws Exception {
		LOG.config("Test: merge changes in some of the contexts.");
		acquireConnection("MixedMultiContextsMergeInSome");
		c.setAutoCommit(false);
		final List<Context> contexts = c.getContexts();
		assertEquals(3, contexts.size());
		final Context ctxB = contexts.get(0);
		final Context ctxD = contexts.get(1);
		final Context ctxIE = contexts.get(2);
		c.persist(entityB.getUri(), entityB, ctxB.getUri());
		c.persist(entityD.getUri(), entityD, ctxD.getUri());
		c.persist(entityA.getUri(), entityA, ctxD.getUri());
		assertNull(entityE.getUri());
		c.persist(entityE.getUri(), entityE, ctxIE.getUri());
		assertNotNull(entityE.getUri());
		c.persist(entityA.getUri(), entityA, ctxIE.getUri());
		c.persist(entityI.getUri(), entityI, ctxIE.getUri());
		c.commit();

		final OWLClassD d = c.find(entityD.getClass(), entityD.getUri());
		assertNotNull(d);
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newA"));
		newA.setStringAttribute("newA'sStringAttribute");
		d.setOwlClassA(newA);
		c.persist(newA.getUri(), newA, ctxD.getUri());
		c.merge(d.getUri(), d);
		final OWLClassE e = c.find(entityE.getClass(), entityE.getUri(), ctxIE.getUri());
		assertNotNull(e);
		final String newString = "newStringAttributeForE";
		e.setStringAttribute(newString);
		c.merge(e.getUri(), e);
		c.commit();

		// Assert that entityA is still present although D is referencing
		// different entity
		assertTrue(c.contains(entityA.getUri(), ctxD.getUri()));
		assertTrue(c.contains(newA.getUri(), ctxD.getUri()));
		// Assert that entityD points to the new OWLClassA
		final OWLClassD resD = c.find(entityD.getClass(), entityD.getUri(), ctxD.getUri());
		assertNotNull(resD);
		assertNotNull(resD.getOwlClassA());
		assertEquals(newA.getUri(), resD.getOwlClassA().getUri());
		assertEquals(newA.getStringAttribute(), resD.getOwlClassA().getStringAttribute());
		// Assert that entityE's string attribute is the new one
		final OWLClassE resE = c.find(entityE.getClass(), entityE.getUri());
		assertNotNull(resE);
		assertEquals(newString, resE.getStringAttribute());
	}

	@Test
	public void testRemoveFromSome() throws Exception {
		LOG.config("Test: remove entity from some context.");
		acquireConnection("MixedMultiContextsRemoveFromSome");
		c.setAutoCommit(false);
		final List<Context> contexts = c.getContexts();
		assertEquals(3, contexts.size());
		final Context ctxA = contexts.get(0);
		final Context ctxBE = contexts.get(1);
		final Context ctxI = contexts.get(2);
		c.persist(entityA.getUri(), entityA, ctxA.getUri());
		c.persist(entityB.getUri(), entityB, ctxBE.getUri());
		assertNull(entityE.getUri());
		c.persist(entityE.getUri(), entityE, ctxBE.getUri());
		assertNotNull(entityE.getUri());
		c.persist(entityI.getUri(), entityI, ctxI.getUri());
		c.persist(entityA.getUri(), entityA, ctxI.getUri());
		c.commit();

		assertTrue(c.contains(entityA.getUri(), ctxA.getUri()));
		assertTrue(c.contains(entityB.getUri(), ctxBE.getUri()));
		assertTrue(c.contains(entityE.getUri(), ctxBE.getUri()));
		assertTrue(c.contains(entityI.getUri(), ctxI.getUri()));
		assertTrue(c.contains(entityA.getUri(), ctxI.getUri()));

		final OWLClassB removeB = c.find(entityB.getClass(), entityB.getUri());
		assertNotNull(removeB);
		final OWLClassA removeA = c.find(entityA.getClass(), entityA.getUri(), ctxI.getUri());
		assertNotNull(removeA);
		final OWLClassI modifyI = c.find(entityI.getClass(), entityI.getUri(), ctxI.getUri());
		assertNotNull(modifyI);
		c.remove(removeB.getUri(), removeB);
		modifyI.setOwlClassA(null);
		c.merge(modifyI.getUri(), modifyI);
		c.remove(removeA.getUri(), removeA, ctxI.getUri());
		c.commit();

		assertFalse(c.contains(entityB.getUri(), ctxBE.getUri()));
		assertFalse(c.contains(entityA.getUri(), ctxI.getUri()));
		assertTrue(c.contains(entityA.getUri(), ctxA.getUri()));
		assertTrue(c.contains(entityE.getUri(), ctxBE.getUri()));
		assertTrue(c.contains(entityI.getUri(), ctxI.getUri()));
		final OWLClassI resI = c.find(entityI.getClass(), entityI.getUri(), ctxI.getUri());
		assertNotNull(resI);
		assertNull(resI.getOwlClassA());
	}

	@Test
	public void testLoadField() throws Exception {
		LOG.config("Test: load field value from a context.");
		acquireConnection("MixedMultiContextsLoadField");
		final List<Context> contexts = c.getContexts();
		assertEquals(3, contexts.size());
		final Context cLoad = contexts.get(1);
		for (Context ctx : contexts) {
			c.persist(entityA.getUri(), entityA, ctx.getUri());
			c.persist(entityI.getUri(), entityI, ctx.getUri());
		}
		c.commit();

		final String changed = "ChangedStringAttribute";
		final OWLClassA changeOne = c.find(entityA.getClass(), entityA.getUri(), contexts.get(0)
				.getUri());
		changeOne.setStringAttribute(changed);
		c.merge(changeOne.getUri(), changeOne);
		final OWLClassA changeTwo = c.find(entityA.getClass(), entityA.getUri(), contexts.get(2)
				.getUri());
		changeTwo.setStringAttribute(changed);
		c.merge(changeTwo.getUri(), changeTwo);
		c.commit();
		final OWLClassA testOne = c.find(entityA.getClass(), entityA.getUri(), contexts.get(0)
				.getUri());
		assertEquals(changed, testOne.getStringAttribute());
		final OWLClassA testTwo = c.find(entityA.getClass(), entityA.getUri(), contexts.get(2)
				.getUri());
		assertEquals(changed, testTwo.getStringAttribute());

		final OWLClassI entI = c.find(entityI.getClass(), entityI.getUri(), cLoad.getUri());
		assertNotNull(entI);
		assertNull(entI.getOwlClassA());
		final Field f = entI.getClass().getDeclaredField(OWLCLASS_A_REFERENCE_FIELD);
		c.loadFieldValue(entI, f);
		assertNotNull(entI.getOwlClassA());
		assertEquals(entityA.getStringAttribute(), entI.getOwlClassA().getStringAttribute());
	}

	private static void acquireConnection(String baseName) throws OntoDriverException {
		ds = TestEnv.createDataSource(baseName, storages);
		c = ds.getConnection(facade);
	}

	private static List<StorageConfig> initStorages() {
		final List<StorageConfig> list = new LinkedList<>();
		list.add(new OwlapiStorageConfig());
		list.add(new OwldbStorageConfig());
		list.add(new JenaStorageConfig());
		return list;
	}
}
