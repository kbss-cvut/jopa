package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.SesameNativeStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.runners.UpdateOperationsRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class TestUpdateOperationsNativeStore {

	private static final Logger LOG = Logger.getLogger(TestUpdateOperationsNativeStore.class
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
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateDataProperty", storage,
				false, properties);
		runner.updateDataPropertyKeepLazyEmpty(em, context());
	}

	@Test
	public void testUpdateDataPropertySetNull() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateDataPropertyToNull",
				storage, true, properties);
		runner.updateDataPropertySetNull(em, context());
	}

	@Test
	public void testUpdateReference() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateReference", storage, true,
				properties);
		runner.updateReference(em, context());
	}

    @Test
    public void testMergeSet() throws Exception {
        em = TestEnvironment.getPersistenceConnector("SesameNativeMergeSet", storage, false, properties);
        runner.mergeSet(em, context());
    }

	@Test
	public void testMergeDetachedWithChanges() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateDetached", storage, true,
				properties);
		runner.mergeDetachedWithChanges(em, context());
	}

	@Test
	public void testMergeDetachedCascade() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateCascade", storage, true,
				properties);
		runner.mergeDetachedCascade(em, context());
	}

    @Test
    public void testMergeDetachedWithObjectPropertyChange() {
        em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateDetachedWithOPChange", storage, true,
                properties);
        runner.mergeDetachedWithUpdatedObjectProperty(em, context());
    }

	@Test
	public void testRemoveFromSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateRemoveFromSimpleList",
				storage, true, properties);
		runner.removeFromSimpleList(em, context());
	}

	@Test
	public void testAddToSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateAddToSimpleList", storage,
				true, properties);
		runner.addToSimpleList(em, context());
	}

	@Test
	public void testClearSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateClearSimpleList", storage,
				true, properties);
		runner.clearSimpleList(em, context());
	}

	@Test
	public void testReplaceSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateReplaceSimpleList",
				storage, true, properties);
		runner.replaceSimpleList(em, context());
	}

	@Test
	public void testRemoveFromReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateRemoveFromReferencedList",
				storage, true, properties);
		runner.removeFromReferencedList(em, context());
	}

	@Test
	public void testAddToReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateAddToReferencedList",
				storage, true, properties);
		runner.addToReferencedList(em, context());
	}

	@Test
	public void testClearReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateClearReferencedList",
				storage, true, properties);
		runner.clearReferencedList(em, context());
	}

	@Test
	public void testReplaceReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateReplaceReferencedList",
				storage, true, properties);
		runner.replaceReferencedList(em, context());
	}

	@Test
	public void testAddNewToProperties() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateAddNewToProperties",
				storage, false, properties);
		runner.addNewToProperties(em, context());
	}

	@Test
	public void testAddPropertyValue() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateAddPropertyValue", storage,
				false, properties);
		runner.addPropertyValue(em, context());
	}

	@Test
	public void testAddPropertyValueDetached() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeUpdateAddPropertyValueDetached",
				storage, false, properties);
		runner.addPropertyValueDetached(em, context());
	}

	@Test(expected = OWLInferredAttributeModifiedException.class)
	public void testModifyInferredAttribute() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeModifyInferredAttribute",
				storage, false, properties);
		runner.modifyInferredAttribute(em, context());
	}

	private URI context() {
		// Don't use contexts for now
		return null;
	}

	private static StorageConfig initStorage() {
		return new SesameNativeStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		map.put("storage", "new");
		return map;
	}
}
