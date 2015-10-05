package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.OwlapiStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.runner.RetrieveOperationsRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class TestRetrieveOperations {

	private static final Logger LOG = Logger.getLogger(TestRetrieveOperations.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private RetrieveOperationsRunner runner;

	private EntityManager em;

	@Before
	public void setUpBeforeClass() throws Exception {
		this.runner = new RetrieveOperationsRunner(LOG);
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
	}

	@Test
	public void testRetrieveSimple() {
		em = TestEnvironment.getPersistenceConnector("OwlapiRetrieveSimple", storage, false,
				properties);
		runner.retrieveSimple(em, context());
	}

	@Test(expected = NullPointerException.class)
	public void testRetrieveNull() {
		em = TestEnvironment.getPersistenceConnector("OwlapiRetrieveNull", storage, false,
				properties);
		runner.retrieveNull(em, context());
	}

	@Test
	public void testRetrieveLazy() throws Exception {
		em = TestEnvironment.getPersistenceConnector("OwlapiRetrieveLazy", storage, false,
				properties);
		runner.retrieveLazily(em, context());
	}

	@Test
	public void testRetrieveGenerated() throws Exception {
		em = TestEnvironment.getPersistenceConnector("OwlapiRetrieveGenerated", storage, false,
				properties);
		runner.retrieveGenerated(em, context());
	}

	@Test
	public void testRetrieveNotExisting() {
		em = TestEnvironment.getPersistenceConnector("OwlapiRetrieveNotExisting", storage, false,
				properties);
		runner.retrieveNotExisting(em, context());
	}

	@Test
	public void testRefresh() {
		em = TestEnvironment.getPersistenceConnector("OwlapiRefresh", storage, false, properties);
		runner.refresh(em, context());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRefreshNotManaged() {
		em = TestEnvironment.getPersistenceConnector("OwlapiRefreshNotManaged", storage, false,
				properties);
		runner.refreshNotManaged(em, context());
	}
	
	@Test
	public void testRetrieveDifferentType() {
		em = TestEnvironment.getPersistenceConnector("OwlapiRetrieveDiffentType", storage, false,
				properties);
		runner.retrieveDifferentType(em, context());
	}

	private URI context() {
		return null;
	}

	private static StorageConfig initStorage() {
		return new OwlapiStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}
