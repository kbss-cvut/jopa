package cz.cvut.kbss.jopa.test.integration.jena;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.RetrieveOperationsRunner;
import cz.cvut.kbss.jopa.test.utils.JenaStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.impl.jena.DriverCachingJenaFactory;

public class TestRetrieveOperationsFile {

	private static final Logger LOG = Logger.getLogger(TestRetrieveOperationsFile.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private static RetrieveOperationsRunner runner;

	private EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		runner = new RetrieveOperationsRunner(LOG);
	}

	@After
	public void tearDown() throws Exception {
		if (em.isOpen()) {
			if (em.getTransaction().isActive()) {
				em.getTransaction().rollback();
			}
			em.close();
			em.getEntityManagerFactory().close();
		}
		runner.initBeforeTest();
	}

	@Test
	public void testRetrieveSimple() {
		em = TestEnvironment.getPersistenceConnector("JenaFileRetrieveSimple", storage, false,
				properties);
		runner.retrieveSimple(em, context());
	}

	@Test(expected = NullPointerException.class)
	public void testRetrieveNull() {
		em = TestEnvironment.getPersistenceConnector("JenaFileRetrieveNull", storage, false,
				properties);
		runner.retrieveNull(em, context());
	}

	@Test
	public void testRetrieveLazy() throws Exception {
		em = TestEnvironment.getPersistenceConnector("JenaFileRetrieveLazy", storage, false,
				properties);
		runner.retrieveLazily(em, context());
	}

	@Test
	public void testRetrieveGenerated() throws Exception {
		em = TestEnvironment.getPersistenceConnector("JenaFileRetrieveGenerated", storage, false,
				properties);
		runner.retrieveGenerated(em, context());
	}

	@Test
	public void testRetrieveNotExisting() {
		em = TestEnvironment.getPersistenceConnector("JenaFileRetrieveNotExisting", storage, false,
				properties);
		runner.retrieveNotExisting(em, context());
	}

	@Test
	public void testRefresh() {
		em = TestEnvironment.getPersistenceConnector("JenaFileRefresh", storage, false, properties);
		runner.refresh(em, context());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRefreshNotManaged() {
		em = TestEnvironment.getPersistenceConnector("JenaFileRefreshNotManaged", storage, false,
				properties);
		runner.refreshNotManaged(em, context());
	}

	private URI context() {
		return null;
	}

	private static StorageConfig initStorage() {
		return new JenaStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.JENA_DRIVER_FACTORY, DriverCachingJenaFactory.class.getName());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}
