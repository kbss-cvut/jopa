package cz.cvut.kbss.jopa.test.owlapi.integration;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.UpdateOperationsRunner;
import cz.cvut.kbss.jopa.test.utils.OwlapiStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class TestUpdateOperations {

	private static final Logger LOG = Logger.getLogger(TestUpdateOperations.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private static UpdateOperationsRunner runner;

	private EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		runner = new UpdateOperationsRunner(LOG);
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
	public void testMergeSet() throws Exception {
		em = TestEnvironment.getPersistenceConnector("OwlapiMergeSet", storage, false, properties);
		runner.mergeSet(em, context());
	}

	@Test
	public void testUpdateDataLeaveLazy() throws Exception {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateDataProperty", storage, false,
				properties);
		runner.updateDataPropertyKeepLazyEmpty(em, context());
	}

	@Test
	public void testUpdateDataPropertySetNull() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateDataPropertyToNull", storage,
				true, properties);
		runner.updateDataPropertySetNull(em, context());
	}

	@Test
	public void testUpdateReference() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateReference", storage, true,
				properties);
		runner.updateReference(em, context());
	}

	@Test
	public void testMergeDetachedWithChanges() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateDetached", storage, true,
				properties);
		runner.mergeDetachedWithChanges(em, context());
	}

	@Test
	public void testMergeDetachedCascade() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateCascade", storage, true,
				properties);
		runner.mergeDetachedCascade(em, context());
	}

	@Test
	public void testRemoveFromSimpleList() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateRemoveFromSimpleList", storage,
				true, properties);
		runner.removeFromSimpleList(em, context());
	}

	@Test
	public void testAddToSimpleList() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateAddToSimpleList", storage, true,
				properties);
		runner.addToSimpleList(em, context());
	}

	@Test
	public void testClearSimpleList() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateClearSimpleList", storage, true,
				properties);
		runner.clearSimpleList(em, context());
	}

	@Test
	public void testReplaceSimpleList() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateReplaceSimpleList", storage,
				true, properties);
		runner.replaceSimpleList(em, context());
	}

	@Test
	public void testRemoveFromReferencedList() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateRemoveFromReferencedList",
				storage, true, properties);
		runner.removeFromReferencedList(em, context());
	}

	@Test
	public void testAddToReferencedList() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateAddToReferencedList", storage,
				true, properties);
		runner.addToReferencedList(em, context());
	}

	@Test
	public void testClearReferencedList() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateClearReferencedList", storage,
				true, properties);
		runner.clearReferencedList(em, context());
	}

	@Test
	public void testReplaceReferencedList() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateReplaceReferencedList", storage,
				true, properties);
		runner.replaceReferencedList(em, context());
	}

	@Test
	public void testAddNewToProperties() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateAddNewToProperties", storage,
				false, properties);
		runner.addNewToProperties(em, context());
	}

	@Test
	public void testAddPropertyValue() {
		em = TestEnvironment.getPersistenceConnector("OwlapiUpdateAddPropertyValue", storage,
				false, properties);
		runner.addPropertyValue(em, context());
	}

	@Test(expected = OWLInferredAttributeModifiedException.class)
	public void testModifyInferredAttribute() {
		em = TestEnvironment.getPersistenceConnector("OwlapiModifyInferredAttribute", storage,
				false, properties);
		runner.modifyInferredAttribute(em, context());
	}

	private URI context() {
		// OWLAPI storages don't use contexts
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
