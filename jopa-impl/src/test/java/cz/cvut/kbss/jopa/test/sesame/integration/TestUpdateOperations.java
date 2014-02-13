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
		runner = new UpdateOperationsRunner();
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
		LOG.config("Test: updates data property. Leaves lazily loaded field empty and checks that after commit the field's value hasn't changed.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateDataProperty", storages, false,
				properties);
		runner.updateDataPropertyKeepLazyEmpty(em, context(1));
	}

	@Test
	public void testUpdateReference() {
		LOG.config("Test: update reference to entity.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReference", storages, true,
				properties);
		runner.updateReference(em, context(0));
	}

	@Test
	public void testMergeDetachedWithChanges() {
		LOG.config("Test: merge detached entity with changes.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateDetached", storages, true,
				properties);
		runner.mergeDetachedWithChanges(em, context(1));
	}

	@Test
	public void testMergeDetachedCascade() {
		LOG.config("Test: merge detached with cascade.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateCascade", storages, true,
				properties);
		runner.mergeDetachedCascade(em, context(0));
	}

	@Test
	public void testRemoveFromSimpleList() {
		LOG.config("Test: remove entity from simple list. (But keep it in the ontology.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateRemoveFromSimpleList", storages,
				true, properties);
		runner.removeFromSimpleList(em, context(1));
	}

	@Test
	public void testAddToSimpleList() {
		LOG.config("Test: add entity to simple list.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddToSimpleList", storages, true,
				properties);
		runner.addToSimpleList(em, context(0));
	}

	@Test
	public void testClearSimpleList() {
		LOG.config("Test: clear a simple list (but keep the entities in ontology).");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateClearSimpleList", storages, true,
				properties);
		runner.clearSimpleList(em, context(1));
	}

	@Test
	public void testReplaceSimpleList() {
		LOG.config("Test: replace simple list with a new one.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReplaceSimpleList", storages,
				true, properties);
		runner.replaceSimpleList(em, context(0));
	}

	@Test
	public void testRemoveFromReferencedList() {
		LOG.config("Test: remove entity from referenced list. (But keep it in the ontology.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateRemoveFromReferencedList",
				storages, true, properties);
		runner.removeFromReferencedList(em, context(1));
	}

	@Test
	public void testAddToReferencedList() {
		LOG.config("Test: add entity to Referenced list.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddToReferencedList", storages,
				true, properties);
		runner.addToReferencedList(em, context(0));
	}

	@Test
	public void testClearReferencedList() {
		LOG.config("Test: clear referenced list (but keep the entities in ontology).");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateClearReferencedList", storages,
				true, properties);
		runner.clearReferencedList(em, context(1));
	}

	@Test
	public void testReplaceReferencedList() {
		LOG.config("Test: replace referenced list with a new one.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateReplaceReferencedList", storages,
				true, properties);
		runner.replaceReferencedList(em, context(0));
	}

	@Test
	public void testAddNewToProperties() {
		LOG.config("Test: add a new property value to entity's properties.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddNewToProperties", storages,
				false, properties);
		runner.addNewToProperties(em, context(1));
	}

	@Test
	public void testAddPropertyValue() {
		LOG.config("Test: add another value to an existing property.");
		em = TestEnvironment.getPersistenceConnector("SesameUpdateAddPropertyValue", storages,
				false, properties);
		runner.addPropertyValue(em, context(0));
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
