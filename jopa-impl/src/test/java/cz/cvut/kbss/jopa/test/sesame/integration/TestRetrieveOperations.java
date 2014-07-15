package cz.cvut.kbss.jopa.test.sesame.integration;

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
import cz.cvut.kbss.jopa.test.utils.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.utils.SesameNativeStorageConfig;
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
		em = TestEnvironment.getPersistenceConnector("SesameRetrieveSimple", storages, false,
				properties);
		runner.retrieveSimple(em, context(1));
	}

	@Test(expected = NullPointerException.class)
	public void testRetrieveNull() {
		em = TestEnvironment.getPersistenceConnector("SesameRetrieveNull", storages, false,
				properties);
		runner.retrieveNull(em, context(0));
	}

	@Test
	public void testRetrieveLazy() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameRetrieveLazy", storages, false,
				properties);
		runner.retrieveLazily(em, context(1));
	}

	@Test
	public void testRetrieveGenerated() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameRetrieveGenerated", storages, false,
				properties);
		runner.retrieveGenerated(em, context(0));
	}

	@Test
	public void testRetrieveNotExisting() {
		em = TestEnvironment.getPersistenceConnector("SesameRetrieveNotExisting", storages, false,
				properties);
		runner.retrieveNotExisting(em, context(1));
	}

	@Test
	public void testRefresh() {
		em = TestEnvironment.getPersistenceConnector("SesameRefresh", storages, false, properties);
		runner.refresh(em, context(0));
	}

	private URI context(int index) {
		return em.getAvailableContexts().get(index).getUri();
	}

	private static List<StorageConfig> initStorages() {
		final List<StorageConfig> lst = new ArrayList<>(2);
		lst.add(new SesameNativeStorageConfig());
		lst.add(new SesameMemoryStorageConfig());
		return lst;
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}
