package cz.cvut.kbss.jopa.test.integration.owldb;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.UpdateOperationsRunner;
import cz.cvut.kbss.jopa.test.utils.OwldbStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

// OWLDB is highly unreliable as a driver and will be removed in the future
@Ignore
public class TestUpdateOperations {

	private static final Logger LOG = Logger.getLogger(TestCreateOperations.class.getName());

	private EntityManager em;

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private UpdateOperationsRunner runner;

	@Before
	public void setUp() throws Exception {
		TestEnvironment.clearDatabase();
		TestEnvironment.resetOwldbHibernateProvider();
		this.runner = new UpdateOperationsRunner(LOG);
	}

	@After
	public void tearDown() throws Exception {
		if (em.getTransaction().isActive()) {
			em.getTransaction().rollback();
		}
		em.getEntityManagerFactory().close();
	}

	@Test
	public void testMergeSet() throws Exception {
		em = TestEnvironment.getPersistenceConnector("OwldbMergeSet", storage, false, properties);
		runner.mergeSet(em, context());
	}

	@Test
	public void testUpdateDataLeaveLazy() throws Exception {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateDataProperty", storage, false,
				properties);
		runner.updateDataPropertyKeepLazyEmpty(em, context());
	}

	@Test
	public void testUpdateDataPropertySetNull() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateDataPropertyToNull", storage,
				true, properties);
		runner.updateDataPropertySetNull(em, context());
	}

	@Test
	public void testUpdateReference() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateReference", storage, true,
				properties);
		runner.updateReference(em, context());
	}

	@Test
	public void testMergeDetachedWithChanges() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateDetached", storage, true,
				properties);
		runner.mergeDetachedWithChanges(em, context());
	}

	@Test
	public void testMergeDetachedCascade() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateCascade", storage, true,
				properties);
		runner.mergeDetachedCascade(em, context());
	}

	@Test
	public void testRemoveFromSimpleList() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateRemoveFromSimpleList", storage,
				true, properties);
		runner.removeFromSimpleList(em, context());
	}

	@Test
	public void testAddToSimpleList() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateAddToSimpleList", storage, true,
				properties);
		runner.addToSimpleList(em, context());
	}

	@Test
	public void testClearSimpleList() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateClearSimpleList", storage, true,
				properties);
		runner.clearSimpleList(em, context());
	}

	@Test
	public void testReplaceSimpleList() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateReplaceSimpleList", storage, true,
				properties);
		runner.replaceSimpleList(em, context());
	}

	@Test
	public void testRemoveFromReferencedList() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateRemoveFromReferencedList",
				storage, true, properties);
		runner.removeFromReferencedList(em, context());
	}

	@Test
	public void testAddToReferencedList() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateAddToReferencedList", storage,
				true, properties);
		runner.addToReferencedList(em, context());
	}

	@Test
	public void testClearReferencedList() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateClearReferencedList", storage,
				true, properties);
		runner.clearReferencedList(em, context());
	}

	@Test
	public void testReplaceReferencedList() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateReplaceReferencedList", storage,
				true, properties);
		runner.replaceReferencedList(em, context());
	}

	@Test
	public void testAddNewToProperties() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateAddNewToProperties", storage,
				false, properties);
		runner.addNewToProperties(em, context());
	}

	@Test
	public void testAddPropertyValue() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateAddPropertyValue", storage, false,
				properties);
		runner.addPropertyValue(em, context());
	}

	@Test
	public void testAddPropertyValueDetached() {
		em = TestEnvironment.getPersistenceConnector("OwldbUpdateAddPropertyValueDetached",
				storage, false, properties);
		runner.addPropertyValueDetached(em, context());
	}

	@Test(expected = OWLInferredAttributeModifiedException.class)
	public void testModifyInferredAttribute() {
		em = TestEnvironment.getPersistenceConnector("OwldbModifyInferredAttribute", storage,
				false, properties);
		runner.modifyInferredAttribute(em, context());
	}

	private URI context() {
		// OWLAPI storages don't use contexts
		return null;
	}

	private static StorageConfig initStorage() {
		return new OwldbStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}
