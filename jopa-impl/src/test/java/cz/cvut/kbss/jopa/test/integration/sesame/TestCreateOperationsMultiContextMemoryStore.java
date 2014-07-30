package cz.cvut.kbss.jopa.test.integration.sesame;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.CreateOperationsMultiContextRunner;
import cz.cvut.kbss.jopa.test.utils.SesameNativeStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class TestCreateOperationsMultiContextMemoryStore {

	private static final Logger LOG = Logger
			.getLogger(TestCreateOperationsMultiContextMemoryStore.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private CreateOperationsMultiContextRunner runner;

	private EntityManager em;

	@Before
	public void setUp() {
		this.runner = new CreateOperationsMultiContextRunner(LOG);
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
	public void testPersistDataPropertyIntoContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameMultiPersistDataPropertyIntoContext",
				storage, false, properties);
		runner.persistDataPropertyIntoContext(em);
	}

	@Test
	public void testPersistObjectPropertyIntoContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameMultiPersistObjectPropertyIntoContext",
				storage, false, properties);
		runner.persistObjectPropertyIntoContext(em);
	}

	@Test
	public void testPersistWithGeneratedIntoContext() {
		em = TestEnvironment.getPersistenceConnector("SesameMultiPersistWithGeneratedIntoContext",
				storage, false, properties);
		runner.persistWithGeneratedIntoContext(em);
	}

	private static StorageConfig initStorage() {
		return new SesameNativeStorageConfig();
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
