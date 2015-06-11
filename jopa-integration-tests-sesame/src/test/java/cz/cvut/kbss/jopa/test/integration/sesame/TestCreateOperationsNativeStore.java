package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.SesameNativeStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.runners.CreateOperationsRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.exceptions.PrimaryKeyNotSetException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class TestCreateOperationsNativeStore {

	private static final Logger LOG = Logger.getLogger(TestCreateOperationsNativeStore.class
			.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private CreateOperationsRunner runner;

	private EntityManager em;

	@Before
	public void setUp() {
		this.runner = new CreateOperationsRunner(LOG);
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
	public void testPersistWithGenerated() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistWithGenerated", storage,
				false, properties);
		runner.persistWithGenerated(em, context());
	}

	@Test(expected = PrimaryKeyNotSetException.class)
	public void testPersistWithoutId() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistWithoutId", storage,
				false, properties);
		runner.persistWithoutId(em, context());
	}

	@Test(expected = NullPointerException.class)
	public void testPersistNull() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistNull", storage, false,
				properties);
		runner.persistNull(em, context());
	}

	@Test
	public void testPersistRollback() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistRollback", storage, false,
				properties);
		runner.persistRollback(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistRollbackOnly() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistRollbackOnly", storage,
				false, properties);
		runner.persistRollbackOnly(em, context());
	}

	@Test
	public void testPersistCascade() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistWithCascade", storage,
				false, properties);
		runner.persistCascade(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistTwiceInOne() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistTwice", storage, false,
				properties);
		runner.persistTwice(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistWithoutCascade() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistWithoutCascade", storage,
				false, properties);
		runner.persistWithoutCascade(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistDetached() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistDetached", storage, false,
				properties);
		runner.persistDetachedEntity(em, context());
	}

	@Test
	public void testPersistSimpleList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistSimpleList", storage,
				false, properties);
		runner.persistSimpleList(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistSimpleListNoCascade() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistSimpleListNoCascade",
				storage, false, properties);
		runner.persistSimpleListNoCascade(em, context());
	}

	@Test
	public void testPersistReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistReferencedList", storage,
				false, properties);
		runner.persistReferencedList(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistReferencedListNoCascade() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistReferencedListNoCascade",
				storage, false, properties);
		runner.persistReferencedListNoCascade(em, context());
	}

	@Test
	public void testPersistSimpleAndReferencedList() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistSimpleAndReferencedList",
				storage, false, properties);
		runner.persistSimpleAndReferencedList(em, context());
	}

	@Test
	public void testPersistProperties() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistWithProperties", storage,
				false, properties);
		runner.persistProperties(em, context());
	}

	@Test
	public void testPersistPropertiesEmpty() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistWithPropertiesEmpty",
				storage, false, properties);
		runner.persistPropertiesEmpty(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void persistURITwiceInDifferentClasses() {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativePersistURITwiceInDifferentClasses", storage, false, properties);
		runner.persistURITwiceInDifferentClasses(em, context());
	}

	@Test
	public void testPersistEntityWithBasicTypeAttributes() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistEntityWithBasicTypeAttributes", storage, false,
				properties);
		runner.persistEntityWithBasicTypedDataProperties(em, context());
	}

	@Test
	public void testPersistAndUpdateAttributeBeforeCommit() {
		em = TestEnvironment.getPersistenceConnector("SesameNativePersistAndUpdateBeforeCommit", storage, false,
				properties);
		runner.setAttributeValueAfterPersistButBeforeCommit(em, context());
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
