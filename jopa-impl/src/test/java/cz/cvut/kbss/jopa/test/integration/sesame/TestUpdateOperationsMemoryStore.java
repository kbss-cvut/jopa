package cz.cvut.kbss.jopa.test.integration.sesame;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.UpdateOperationsRunner;
import cz.cvut.kbss.jopa.test.utils.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class TestUpdateOperationsMemoryStore {

	private static final Logger LOG = Logger.getLogger(TestUpdateOperationsMemoryStore.class
			.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private UpdateOperationsRunner runner;

	private EntityManager em;

	@Before
	public void setUp() throws Exception {
		this.runner = new UpdateOperationsRunner(LOG);
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
	public void testUpdateDataLeaveLazy() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateDataProperty", storage, false,
				properties);
		runner.updateDataPropertyKeepLazyEmpty(em, context());
	}

	@Test
	public void testUpdateDataPropertySetNull() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateDataPropertyToNull", storage,
				true, properties);
		runner.updateDataPropertySetNull(em, context());
	}

	@Test
	public void testUpdateReference() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReference", storage, true,
				properties);
		runner.updateReference(em, context());
	}

	@Test
	public void testMergeDetachedWithChanges() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateDetached", storage, true,
				properties);
		runner.mergeDetachedWithChanges(em, context());
	}

	@Test
	public void testMergeDetachedCascade() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateCascade", storage, true,
				properties);
		runner.mergeDetachedCascade(em, context());
	}

	@Test
	public void testRemoveFromSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateRemoveFromSimpleList", storage,
				true, properties);
		runner.removeFromSimpleList(em, context());
	}

	@Test
	public void testAddToSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddToSimpleList", storage, true,
				properties);
		runner.addToSimpleList(em, context());
	}

	@Test
	public void testClearSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateClearSimpleList", storage, true,
				properties);
		runner.clearSimpleList(em, context());
	}

	@Test
	public void testReplaceSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReplaceSimpleList", storage,
				true, properties);
		runner.replaceSimpleList(em, context());
	}

	@Test
	public void testRemoveFromReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateRemoveFromReferencedList",
				storage, true, properties);
		runner.removeFromReferencedList(em, context());
	}

	@Test
	public void testAddToReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddToReferencedList", storage,
				true, properties);
		runner.addToReferencedList(em, context());
	}

	@Test
	public void testClearReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateClearReferencedList", storage,
				true, properties);
		runner.clearReferencedList(em, context());
	}

	@Test
	public void testReplaceReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReplaceReferencedList", storage,
				true, properties);
		runner.replaceReferencedList(em, context());
	}

	@Test
	public void testAddNewToProperties() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddNewToProperties", storage,
				false, properties);
		runner.addNewToProperties(em, context());
	}

	@Test
	public void testAddPropertyValue() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddPropertyValue", storage,
				false, properties);
		runner.addPropertyValue(em, context());
	}

	@Test
	public void testAddPropertyValueDetached() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddPropertyValueDetached",
				storage, false, properties);
		runner.addPropertyValueDetached(em, context());
	}

	@Test(expected = OWLInferredAttributeModifiedException.class)
	public void testModifyInferredAttribute() {
		em = TestEnvironment.getPersistenceConnector("SesameModifyInferredAttribute", storage,
				false, properties);
		runner.modifyInferredAttribute(em, context());
	}

	private URI context() {
		// Don't use contexts for now
		return null;
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
