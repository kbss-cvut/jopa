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
import cz.cvut.kbss.jopa.test.integration.runners.UpdateOperationsMultiContextRunner;
import cz.cvut.kbss.jopa.test.utils.SesameNativeStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class TestUpdateOperationsMultiContextNativeStore {

	private static final Logger LOG = Logger
			.getLogger(TestUpdateOperationsMultiContextNativeStore.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private UpdateOperationsMultiContextRunner runner;

	private EntityManager em;

	@Before
	public void setUp() {
		this.runner = new UpdateOperationsMultiContextRunner(LOG);
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
	public void testUpdateDataPropertyInContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiUpdateDataPropertyInContext", storage, false, properties);
		runner.updateDataPropertyInContext(em);
	}

	@Test
	public void testUpdateObjectPropertyToDifferentContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiUpdateObjectPropertyToDifferent", storage, false, properties);
		runner.updateObjectPropertyToDifferentContext(em);
	}

	@Test
	public void testUpdateAddToPropertiesInContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiUpdateAddToPropertiesInContext", storage, false, properties);
		runner.updateAddToPropertiesInContext(em);
	}

	@Test
	public void testUpdateAddToSimpleListInContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiUpdateAddToSimpleListInContext", storage, false, properties);
		runner.updateAddToSimpleListInContext(em);
	}

	@Test
	public void testUpdateAddToReferencedListInContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiUpdateAddToReferencedListInContext", storage, false, properties);
		runner.updateAddToReferencedListInContext(em);
	}

	@Test
	public void testUpdateRemoveFromSimpleListInContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiUpdateRemoveFromSimpleListInContext", storage, false, properties);
		runner.updateRemoveFromSimpleListInContext(em);
	}

	@Test
	public void testUpdateRemoveFromReferencedListInContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiUpdateRemoveFromReferencedListInContext", storage, false,
				properties);
		runner.updateRemoveFromReferencedListInContext(em);
	}

	private static StorageConfig initStorage() {
		return new SesameNativeStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}
