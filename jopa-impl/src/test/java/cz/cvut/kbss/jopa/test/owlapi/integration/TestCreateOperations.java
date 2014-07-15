package cz.cvut.kbss.jopa.test.owlapi.integration;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.CreateOperationsRunner;
import cz.cvut.kbss.jopa.test.utils.OwlapiStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.exceptions.PrimaryKeyNotSetException;

public class TestCreateOperations {

	private static final Logger LOG = Logger.getLogger(TestCreateOperations.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private CreateOperationsRunner runner;

	private EntityManager em;

	@Before
	public void setUp() throws Exception {
		runner = new CreateOperationsRunner(LOG);
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
	public void testPersistWithGenerated() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistWithGenerated", storage, false,
				properties);
		runner.persistWithGenerated(em, context());
	}

	@Test(expected = PrimaryKeyNotSetException.class)
	public void testPersistWithoutId() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistWithoutId", storage, false,
				properties);
		runner.persistWithoutId(em, context());
	}

	@Test(expected = NullPointerException.class)
	public void testPersistNull() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistNull", storage, false,
				properties);
		runner.persistNull(em, context());
	}

	@Test
	public void testPersistRollback() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistRollback", storage, false,
				properties);
		runner.persistRollback(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistRollbackOnly() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistRollbackOnly", storage, false,
				properties);
		runner.persistRollbackOnly(em, context());
	}

	@Test
	public void testPersistCascade() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistWithCascade", storage, false,
				properties);
		runner.persistCascade(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistTwiceInOne() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistTwice", storage, false,
				properties);
		runner.persistTwice(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistWithoutCascade() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistWithoutCascade", storage, false,
				properties);
		runner.persistWithoutCascade(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistDetached() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistDetached", storage, false,
				properties);
		runner.persistDetachedEntity(em, context());
	}

	@Test
	public void testPersistSimpleList() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistSimpleList", storage, false,
				properties);
		runner.persistSimpleList(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistSimpleListNoCascade() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistSimpleListNoCascade", storage,
				false, properties);
		runner.persistSimpleListNoCascade(em, context());
	}

	@Test
	public void testPersistReferencedList() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistReferencedList", storage, false,
				properties);
		runner.persistReferencedList(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistReferencedListNoCascade() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistReferencedListNoCascade",
				storage, false, properties);
		runner.persistReferencedListNoCascade(em, context());
	}

	@Test
	public void testPersistSimpleAndReferencedList() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistSimpleAndReferencedList",
				storage, false, properties);
		runner.persistSimpleAndReferencedList(em, context());
	}

	@Test
	public void testPersistProperties() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistWithProperties", storage, false,
				properties);
		runner.persistProperties(em, context());
	}

	@Test
	public void testPersistPropertiesEmpty() {
		em = TestEnvironment.getPersistenceConnector("OwlapiPersistWithPropertiesEmpty", storage,
				false, properties);
		runner.persistPropertiesEmpty(em, context());
	}

	private URI context() {
		// OWLAPI doesn't use contexts
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
