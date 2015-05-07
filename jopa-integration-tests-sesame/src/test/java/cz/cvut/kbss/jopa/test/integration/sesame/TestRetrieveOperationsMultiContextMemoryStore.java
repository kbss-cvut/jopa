package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.runners.RetrieveOperationsMultiContextRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class TestRetrieveOperationsMultiContextMemoryStore {

	private static final Logger LOG = Logger
			.getLogger(TestRetrieveOperationsMultiContextMemoryStore.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private RetrieveOperationsMultiContextRunner runner;

	private EntityManager em;

	@Before
	public void setUp() {
		this.runner = new RetrieveOperationsMultiContextRunner(LOG);
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
	public void testRetrieveSimilarFromTwoContexts() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameMultiRetrieveSimilarFromTwoContexts",
				storage, false, properties);
		runner.retrieveSimilarFromTwoContexts(em);
	}

	@Test
	public void testRetrieveSimpleListFromContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameMultiRetrieveSimpleListFromContext",
				storage, false, properties);
		runner.retrieveSimpleListFromContext(em);
	}

	@Test
	public void testRetrieveReferencedListFromContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameMultiRetrieveReferencedListFromContext", storage, false, properties);
		runner.retrieveReferencedlistFromContext(em);
	}

	@Test
	public void testRetrieveLazyReferenceFromContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameMultiRetrieveLazyReferenceFromContext",
				storage, false, properties);
		runner.retrieveLazyReferenceFromContext(em);
	}

	@Test
	public void testRetrievePropertiesFromContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameMultiRetrievePropertiesFromContext",
				storage, false, properties);
		runner.retrievePropertiesFromContext(em);
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
