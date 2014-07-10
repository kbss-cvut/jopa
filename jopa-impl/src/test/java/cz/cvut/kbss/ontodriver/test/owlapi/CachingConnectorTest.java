package cz.cvut.kbss.ontodriver.test.owlapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassI;
import cz.cvut.kbss.jopa.test.utils.OwlapiStorageConfig;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.jena.DriverCachingJenaFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverCachingOwlapiFactory;
import cz.cvut.kbss.ontodriver.test.BaseSingleContextOntoDriverTest;
import cz.cvut.kbss.ontodriver.test.TestEnv;

public class CachingConnectorTest extends BaseSingleContextOntoDriverTest {

	private static final Map<String, String> properties = initProperties();

	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;
	// Generated IRI
	private static OWLClassE entityE;
	// Lazy reference to OWLClassA
	private static OWLClassI entityI;

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
		storageConfig = new OwlapiStorageConfig();
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
	public void testPersist() throws Exception {
		LOG.config("Test: persist into both contexts some entities.");
		acquireConnection("cachingPersistIntoAll");
		c.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		persist(entityD.getUri(), entityD);
		persist(entityB.getUri(), entityB);
		persist(entityE.getUri(), entityE);
		assertNotNull(entityE.getUri());
		c.commit();

		assertNotNull(find(OWLClassA.class, entityA.getUri()));
		assertNotNull(find(OWLClassD.class, entityD.getUri()));
		assertTrue(contains(entityB.getUri()));
		assertNotNull(find(OWLClassB.class, entityB.getUri()));
		assertNotNull(find(OWLClassE.class, entityE.getUri()));
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testConcurrentConnectionsPersist() throws Exception {
		LOG.config("Test: open two connections and persist entities in both.");
		acquireConnection("cachingConcurrentConnectionsPersist");
		cTwo = ds.getConnection(facade);
		c.setAutoCommit(false);
		cTwo.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		assertTrue(contains(entityA.getUri()));
		assertFalse(cTwo.contains(entityA.getUri(), null));
		cTwo.persist(entityB.getUri(), entityB, DEFAULT_DESCRIPTOR);
		assertTrue(cTwo.contains(entityB.getUri(), null));
		assertFalse(c.contains(entityB.getUri(), null));
		persist(entityA.getUri(), entityA);
		persist(entityI.getUri(), entityI);
		c.commit();
		cTwo.commit();

		assertNotNull(find(OWLClassA.class, entityA.getUri()));
		assertNotNull(cTwo.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR));
		assertNotNull(find(OWLClassB.class, entityB.getUri()));
		assertNotNull(cTwo.find(OWLClassB.class, entityB.getUri(), DEFAULT_DESCRIPTOR));
		assertNotNull(find(OWLClassA.class, entityA.getUri()));
		assertNotNull(cTwo.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR));
		assertNotNull(find(OWLClassI.class, entityI.getUri()));
		assertNotNull(cTwo.find(OWLClassI.class, entityI.getUri(), DEFAULT_DESCRIPTOR));
	}

	@Test
	public void testModifyConcurrently() throws Exception {
		LOG.config("Test: modify an attribute in two concurrently open connections. Tests transaction isolation.");
		acquireConnection("cachingConcurrentConnectionsModify");
		c.setAutoCommit(false);
		persist(entityA.getUri(), entityA);
		c.commit();

		cTwo = ds.getConnection(facade);
		cTwo.setAutoCommit(false);
		final OWLClassA cA = find(OWLClassA.class, entityA.getUri());
		assertNotNull(cA);
		final OWLClassA cTwoA = cTwo.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
		assertNotNull(cTwoA);
		final String older = "olderString";
		final String newer = "newerString";
		cTwoA.setStringAttribute(older);
		cA.setStringAttribute(newer);
		final Field field = OWLClassA.getStrAttField();
		cTwo.merge(cTwoA, field, DEFAULT_DESCRIPTOR);
		merge(cA, field);
		cTwo.commit();
		assertEquals(newer, cA.getStringAttribute());
		c.commit();

		final OWLClassA resC = find(OWLClassA.class, entityA.getUri());
		assertNotNull(resC);
		assertEquals(newer, resC.getStringAttribute());
		final OWLClassA resCTwo = cTwo.find(OWLClassA.class, entityA.getUri(), DEFAULT_DESCRIPTOR);
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
		final OWLClassE anotherE = new OWLClassE();
		anotherE.setStringAttribute("another'sEStringAttribute");
		persist(null, entityE);
		assertNotNull(entityE.getUri());
		cTwo.persist(null, anotherE, DEFAULT_DESCRIPTOR);
		assertNotNull(anotherE.getUri());
		assertFalse(entityE.getUri().equals(anotherE.getUri()));
		c.commit();
		cTwo.commit();

		assertNotNull(find(OWLClassE.class, entityE.getUri()));
		assertNotNull(cTwo.find(OWLClassE.class, entityE.getUri(), DEFAULT_DESCRIPTOR));
		final OWLClassE resC = find(OWLClassE.class, anotherE.getUri());
		assertNotNull(resC);
		assertEquals(anotherE.getStringAttribute(), resC.getStringAttribute());
		final OWLClassE resCTwo = find(OWLClassE.class, anotherE.getUri());
		assertNotNull(resCTwo);
		assertEquals(anotherE.getStringAttribute(), resCTwo.getStringAttribute());
	}

	@Override
	protected void acquireConnection(String baseName) throws OntoDriverException {
		ds = TestEnv.createDataSource(baseName, storageConfig, properties);
		c = ds.getConnection(facade);
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> m = new HashMap<String, String>();
		m.put(OntoDriverProperties.OWLAPI_DRIVER_FACTORY,
				DriverCachingOwlapiFactory.class.getName());
		m.put(OntoDriverProperties.JENA_DRIVER_FACTORY, DriverCachingJenaFactory.class.getName());
		return m;
	}
}
