package cz.cvut.kbss.jopa.test.owlapi.integration;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.RetrieveOperationsRunner;
import cz.cvut.kbss.jopa.test.utils.OwlapiStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class TestRetrieveOperations {

	private static final Logger LOG = Logger.getLogger(TestRetrieveOperations.class.getName());

	private static final List<StorageConfig> storages = initStorages();
	private static final Map<String, String> properties = initProperties();

	private static RetrieveOperationsRunner runner;

	private EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		runner = new RetrieveOperationsRunner();
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
	public void testRetrieveLazy() throws Exception {
		LOG.config("Test: retrieve entity with lazy loaded attribute.");
		em = TestEnvironment.getPersistenceConnector("OwlapiRetrieveLazy", storages, false,
				properties);
		runner.retrieveLazily(em, context());
	}

	@Test
	public void testRetrieveGenerated() throws Exception {
		LOG.config("Test: persist and retrieve several entities with generated identifiers.");
		em = TestEnvironment.getPersistenceConnector("OwlapiRetrieveGenerated", storages, false,
				properties);
		runner.retrieveGenerated(em, context());
	}

	@Test
	public void testRetrieveNotExisting() {
		LOG.config("Test: retrieve entity which does not exist in the specified context.");
		em = TestEnvironment.getPersistenceConnector("OwlapiRetrieveNotExisting", storages, false,
				properties);
		runner.retrieveNotExisting(em, context());
	}

	@Test
	public void testRefresh() {
		LOG.config("Test: refresh entity.");
		em = TestEnvironment.getPersistenceConnector("OwlapiRefresh", storages, false, properties);
		runner.refresh(em, context());
	}

	private URI context() {
		return em.getAvailableContexts().get(0).getUri();
	}

	private static List<StorageConfig> initStorages() {
		final List<StorageConfig> lst = new ArrayList<>(1);
		lst.add(new OwlapiStorageConfig());
		return lst;
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}
