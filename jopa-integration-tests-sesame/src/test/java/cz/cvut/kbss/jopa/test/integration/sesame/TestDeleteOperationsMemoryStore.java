package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.runners.DeleteOperationsRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class TestDeleteOperationsMemoryStore {

	private static final Logger LOG = Logger.getLogger(TestDeleteOperationsMemoryStore.class
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
	}

	@Test
	public void testRemoveSimple() {
		em = TestEnvironment.getPersistenceConnector("SesameSimpleRemove", storage, false,
				properties);
		runner.removeSimple(em, context());
	}

	// TODO First we need to resolve referential integrity
	@Ignore
	@Test
	public void testRemoveReference() {
		em = TestEnvironment.getPersistenceConnector("SesameRemoveReference", storage, true,
				properties);
		runner.removeReference(em, context());
	}

	@Test
	public void testRemoveCascade() {
		em = TestEnvironment.getPersistenceConnector("SesameRemoveCascade", storage, true,
				properties);
		runner.removeCascade(em, context());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testRemoveDetached() {
		em = TestEnvironment.getPersistenceConnector("SesameRemoveDetached", storage, true,
				properties);
		runner.removeDetached(em, context());
	}

	@Test
	public void testRemoveFromSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameRemoveFromSimpleList", storage, true,
				properties);
		runner.removeFromSimpleList(em, context());
	}

	@Test
	public void testRemoveFromReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameRemoveFromReferencedList", storage,
				true, properties);
		runner.removeFromReferencedList(em, context());
	}

	@Test
	public void testRemoveListOwner() {
		em = TestEnvironment.getPersistenceConnector("SesameRemoveListOwner", storage, true,
				properties);
		runner.removeListOwner(em, context());
	}

	@Test
	public void testRemoveNotYetCommitted() {
		em = TestEnvironment.getPersistenceConnector("SesameRemoveNotYetCommitted", storage, true,
				properties);
		runner.removeNotYetCommitted(em, context());
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
