package cz.cvut.kbss.jopa.ontodriver.integration.owlapi;

import static org.junit.Assert.*;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

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
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.jena.DriverCachingJenaFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverCachingOwlapiFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public class CachingConnectorsTest {

	private static final Logger LOG = Logger.getLogger(SingleFileContextTest.class.getName());

	private static final List<StorageInfo> storage = initStorages();
	private static final String OWLCLASS_A_REFERENCE_FIELD = "owlClassA";
	private static final Map<String, String> properties = initProperties();

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
	private static Connection cTwo;

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
		facade = TestEnv.getProviderFacade();
	}

	@After
	public void tearDown() throws Exception {
		if (c != null) {
			c.close();
		}
		if (cTwo != null) {
			cTwo.close();
			cTwo = null;
		}
		entityE.setUri(null);
	}

	@Test
	public void testPersistIntoAll() throws Exception {
		LOG.config("Test: persist into both contexts some entities.");
		acquireConnection("cachingPersistIntoAll");
		c.setAutoCommit(false);
		final List<Context> contexts = c.getContexts();
		assertEquals(storage.size(), contexts.size());
		final Context c0 = contexts.get(0);
		final Context c1 = contexts.get(1);
		c.persist(entityA.getUri(), entityA, c1.getUri());
		c.persist(entityD.getUri(), entityD, c1.getUri());
		c.persist(entityB.getUri(), entityB, c0.getUri());
		c.persist(entityE.getUri(), entityE, c0.getUri());
		assertNotNull(entityE.getUri());
		c.commit();

		assertNotNull(c.find(OWLClassA.class, entityA.getUri(), c1.getUri()));
		assertNotNull(c.find(OWLClassD.class, entityD.getUri(), c1.getUri()));
		assertTrue(c.contains(entityB.getUri(), c0.getUri()));
		assertNotNull(c.find(OWLClassB.class, entityB.getUri(), c0.getUri()));
		assertNotNull(c.find(OWLClassE.class, entityE.getUri(), c0.getUri()));
	}

	@Test
	public void testConcurrentConnectionsPersist() throws Exception {
		LOG.config("Test: open two connections and persist entities in both.");
		acquireConnection("cachingConcurrentConnectionsPersist");
		cTwo = ds.getConnection(facade);
		c.setAutoCommit(false);
		cTwo.setAutoCommit(false);
		final Context c0 = c.getContexts().get(0);
		final Context c1 = c.getContexts().get(1);
		assertEquals(c0, cTwo.getContext(c0.getUri()));
		assertEquals(c1, cTwo.getContext(c1.getUri()));
		c.persist(entityA.getUri(), entityA, c0.getUri());
		assertTrue(c.contains(entityA.getUri(), c0.getUri()));
		assertFalse(cTwo.contains(entityA.getUri(), c0.getUri()));
		cTwo.persist(entityB.getUri(), entityB, c0.getUri());
		assertTrue(cTwo.contains(entityB.getUri(), c0.getUri()));
		assertFalse(c.contains(entityB.getUri(), c0.getUri()));
		c.persist(entityA.getUri(), entityA, c1.getUri());
		c.persist(entityI.getUri(), entityI, c1.getUri());
		c.commit();
		cTwo.commit();

		assertNotNull(c.find(OWLClassA.class, entityA.getUri(), c0.getUri()));
		assertNotNull(cTwo.find(OWLClassA.class, entityA.getUri(), c0.getUri()));
		assertNotNull(c.find(OWLClassB.class, entityB.getUri(), c0.getUri()));
		assertNotNull(cTwo.find(OWLClassB.class, entityB.getUri(), c0.getUri()));
		assertNotNull(c.find(OWLClassA.class, entityA.getUri(), c1.getUri()));
		assertNotNull(cTwo.find(OWLClassA.class, entityA.getUri(), c1.getUri()));
		assertNotNull(c.find(OWLClassI.class, entityI.getUri(), c1.getUri()));
		assertNotNull(cTwo.find(OWLClassI.class, entityI.getUri(), c1.getUri()));
	}

	@Test
	public void testModifyConcurrently() throws Exception {
		LOG.config("Test: modify an attribute in two concurrently open connections.");
		acquireConnection("cachingConcurrentConnectionsModify");
		c.setAutoCommit(false);
		final Context ctx = c.getContexts().get(0);
		final Context ctx2 = c.getContexts().get(1);
		c.persist(entityA.getUri(), entityA, ctx.getUri());
		c.persist(entityA.getUri(), entityA, ctx2.getUri());
		c.commit();

		cTwo = ds.getConnection(facade);
		cTwo.setAutoCommit(false);
		final OWLClassA cA = c.find(OWLClassA.class, entityA.getUri(), ctx.getUri());
		assertNotNull(cA);
		final OWLClassA cTwoA = cTwo.find(OWLClassA.class, entityA.getUri(), ctx2.getUri());
		assertNotNull(cTwoA);
		final String older = "olderString";
		final String newer = "newerString";
		cTwoA.setStringAttribute(older);
		cA.setStringAttribute(newer);
		cTwo.merge(cTwoA.getUri(), cTwoA);
		c.merge(cA.getUri(), cA);
		cTwo.commit();
		assertEquals(newer, cA.getStringAttribute());
		c.commit();

		final OWLClassA resC = c.find(OWLClassA.class, entityA.getUri(), ctx.getUri());
		assertNotNull(resC);
		assertEquals(newer, resC.getStringAttribute());
		final OWLClassA resCTwo = cTwo.find(OWLClassA.class, entityA.getUri(), ctx.getUri());
		assertNotNull(resCTwo);
		assertEquals(newer, resCTwo.getStringAttribute());
	}

	@Test
	public void testPersistWithPkGeneration() throws Exception {
		LOG.config("Test: persist entities with generated pk concurrently in two connections. Assert the keys are different.");
		acquireConnection("cachingConcurrentPersistGeneratePk");
		c.setAutoCommit(false);
		cTwo = ds.getConnection(facade);
		cTwo.setAutoCommit(false);
		final Context ctx = c.getContexts().get(c.getContexts().size() - 1);
		final OWLClassE anotherE = new OWLClassE();
		anotherE.setStringAttribute("another'sEStringAttribute");
		c.persist(null, entityE, ctx.getUri());
		assertNotNull(entityE.getUri());
		cTwo.persist(null, anotherE, ctx.getUri());
		assertNotNull(anotherE.getUri());
		assertFalse(entityE.getUri().equals(anotherE.getUri()));
		c.commit();
		cTwo.commit();

		assertNotNull(c.find(OWLClassE.class, entityE.getUri(), ctx.getUri()));
		assertNotNull(cTwo.find(OWLClassE.class, entityE.getUri(), ctx.getUri()));
		final OWLClassE resC = c.find(OWLClassE.class, anotherE.getUri(), ctx.getUri());
		assertNotNull(resC);
		assertEquals(anotherE.getStringAttribute(), resC.getStringAttribute());
		final OWLClassE resCTwo = c.find(OWLClassE.class, anotherE.getUri(), ctx.getUri());
		assertNotNull(resCTwo);
		assertEquals(anotherE.getStringAttribute(), resCTwo.getStringAttribute());
	}

	private static void acquireConnection(String baseName) throws OntoDriverException {
		ds = TestEnv.createDataSource(baseName, storage, properties);
		c = ds.getConnection(facade);
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> m = new HashMap<String, String>();
		m.put(OntoDriverProperties.OWLAPI_DRIVER_FACTORY,
				DriverCachingOwlapiFactory.class.getName());
		m.put(OntoDriverProperties.JENA_DRIVER_FACTORY, DriverCachingJenaFactory.class.getName());
		return m;
	}

	private static List<StorageInfo> initStorages() {
		final List<StorageInfo> lst = new ArrayList<StorageInfo>(2);
		lst.add(new StorageInfo(OntologyConnectorType.OWLAPI, OwlapiStorageType.FILE));
		lst.add(new StorageInfo(OntologyConnectorType.JENA, OwlapiStorageType.FILE));
		return lst;
	}
}
