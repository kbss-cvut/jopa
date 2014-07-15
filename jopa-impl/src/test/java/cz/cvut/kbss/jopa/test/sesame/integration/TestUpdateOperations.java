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

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.UpdateOperationsRunner;
import cz.cvut.kbss.jopa.test.utils.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.utils.SesameNativeStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class TestUpdateOperations {

	private static final Logger LOG = Logger.getLogger(TestUpdateOperations.class.getName());

	private static final List<StorageConfig> storages = initStorages();
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
	public void testUpdateDataLeaveLazy() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateDataProperty", storages, false,
				properties);
		runner.updateDataPropertyKeepLazyEmpty(em, context(0));
	}

	@Test
	public void testUpdateDataPropertySetNull() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateDataPropertyToNull", storages,
				true, properties);
		runner.updateDataPropertySetNull(em, context(1));
	}

	@Test
	public void testUpdateReference() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReference", storages, true,
				properties);
		runner.updateReference(em, context(0));
	}

	@Test
	public void testMergeDetachedWithChanges() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateDetached", storages, true,
				properties);
		runner.mergeDetachedWithChanges(em, context(1));
	}

	@Test
	public void testMergeDetachedCascade() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateCascade", storages, true,
				properties);
		runner.mergeDetachedCascade(em, context(0));
	}

	@Test
	public void testRemoveFromSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateRemoveFromSimpleList", storages,
				true, properties);
		runner.removeFromSimpleList(em, context(1));
	}

	@Test
	public void testAddToSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddToSimpleList", storages, true,
				properties);
		runner.addToSimpleList(em, context(0));
	}

	@Test
	public void testClearSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateClearSimpleList", storages, true,
				properties);
		runner.clearSimpleList(em, context(1));
	}

	@Test
	public void testReplaceSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReplaceSimpleList", storages,
				true, properties);
		runner.replaceSimpleList(em, context(0));
	}

	@Test
	public void testRemoveFromReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateRemoveFromReferencedList",
				storages, true, properties);
		runner.removeFromReferencedList(em, context(1));
	}

	@Test
	public void testAddToReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddToReferencedList", storages,
				true, properties);
		runner.addToReferencedList(em, context(0));
	}

	@Test
	public void testClearReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateClearReferencedList", storages,
				true, properties);
		runner.clearReferencedList(em, context(1));
	}

	@Test
	public void testReplaceReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReplaceReferencedList", storages,
				true, properties);
		runner.replaceReferencedList(em, context(0));
	}

	@Test
	public void testAddNewToProperties() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddNewToProperties", storages,
				false, properties);
		runner.addNewToProperties(em, context(1));
	}

	@Test
	public void testAddPropertyValue() {
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddPropertyValue", storages,
				false, properties);
		runner.addPropertyValue(em, context(0));
	}

	@Test(expected = OWLInferredAttributeModifiedException.class)
	public void testModifyInferredAttribute() {
		em = TestEnvironment.getPersistenceConnector("SesameModifyInferredAttribute", storages,
				false, properties);
		runner.modifyInferredAttribute(em, context(1));
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
