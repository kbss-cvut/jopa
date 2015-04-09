package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.DeleteOperationsMultiContextRunner;
import cz.cvut.kbss.jopa.test.environment.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class TestDeleteOperationsMultiContextMemoryStore {

	private static final Logger LOG = Logger
			.getLogger(TestDeleteOperationsMultiContextMemoryStore.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private DeleteOperationsMultiContextRunner runner;

	private EntityManager em;

	@Before
	public void setUp() {
		this.runner = new DeleteOperationsMultiContextRunner(LOG);
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
	public void testRemoveFromContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameMultiRemoveFromContext", storage,
				false, properties);
		runner.removeFromContext(em);
	}

	@Test
	public void testRemoveFromOneKeepInTheOther() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameMultiRemoveFromOneContextAndKeepInTheOther", storage, false, properties);
		runner.removeFromOneKeepInTheOther(em);
	}

	@Test
	public void testRemoveObjectPropertyFromContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameMultiRemoveObjectPropertyFromContext",
				storage, false, properties);
		runner.removeObjectPropertyFromContext(em);
	}

	@Test
	public void testRemoveCascadeOverContexts() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameMultiRemoveCascadeOverContexts",
				storage, false, properties);
		runner.removeCascadeOverContexts(em);
	}

	private static StorageConfig initStorage() {
		return new SesameMemoryStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		map.put("storage", "new");
		return map;
	}
}
