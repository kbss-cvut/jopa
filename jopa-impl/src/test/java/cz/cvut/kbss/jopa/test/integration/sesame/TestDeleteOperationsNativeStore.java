package cz.cvut.kbss.jopa.test.integration.sesame;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.DeleteOperationsRunner;
import cz.cvut.kbss.jopa.test.utils.SesameNativeStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class TestDeleteOperationsNativeStore {

	private static final Logger LOG = Logger.getLogger(TestDeleteOperationsNativeStore.class
			.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private DeleteOperationsRunner runner;

	private EntityManager em;

	@Before
	public void setUp() {
		this.runner = new DeleteOperationsRunner(LOG);
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
	public void testRemoveSimple() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeSimpleRemove", storage, false,
				properties);
		runner.removeSimple(em, context());
	}

	@Test
	public void testRemoveReference() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeRemoveReference", storage, true,
				properties);
		runner.removeReference(em, context());
	}

	@Test
	public void testRemoveCascade() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeRemoveCascade", storage, true,
				properties);
		runner.removeCascade(em, context());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRemoveDetached() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeRemoveDetached", storage, true,
				properties);
		runner.removeDetached(em, context());
	}

	@Test
	public void testRemoveFromSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeRemoveFromSimpleList", storage,
				true, properties);
		runner.removeFromSimpleList(em, context());
	}

	@Test
	public void testRemoveFromReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeRemoveFromReferencedList",
				storage, true, properties);
		runner.removeFromReferencedList(em, context());
	}

	@Test
	public void testRemoveListOwner() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeRemoveListOwner", storage, true,
				properties);
		runner.removeListOwner(em, context());
	}

	@Test
	public void testRemoveNotYetCommitted() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeRemoveNotYetCommitted", storage,
				true, properties);
		runner.removeNotYetCommitted(em, context());
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
		return map;
	}
}
