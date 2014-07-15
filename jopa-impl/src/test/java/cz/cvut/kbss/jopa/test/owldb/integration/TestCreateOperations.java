package cz.cvut.kbss.jopa.test.owldb.integration;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.CreateOperationsRunner;
import cz.cvut.kbss.jopa.test.utils.OwldbStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.exceptions.PrimaryKeyNotSetException;

public class TestCreateOperations {

	private static final Logger LOG = Logger.getLogger(TestCreateOperations.class.getName());

	private static EntityManager em;

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private CreateOperationsRunner runner;

	@BeforeClass
	public static void setupBeforeClass() throws Exception {
		TestEnvironment.clearDatabase();
		TestEnvironment.resetOwldbHibernateProvider();
		em = TestEnvironment.getPersistenceConnector("OWLDBPersistenceTest-create", storage, true,
				properties);
	}

	@Before
	public void setUp() throws Exception {
		TestEnvironment.clearDatabase();
		this.runner = new CreateOperationsRunner(LOG);
	}

	@After
	public void tearDown() throws Exception {
		if (em.getTransaction().isActive()) {
			em.getTransaction().rollback();
		}
	}

	@AfterClass
	public static void teardownAfterClass() {
		em.getEntityManagerFactory().close();
	}

	@Test
	public void testPersistWithGenerated() {
		runner.persistWithGenerated(em, context());
	}

	@Test(expected = PrimaryKeyNotSetException.class)
	public void testPersistWithoutId() {
		runner.persistWithoutId(em, context());
	}

	@Test(expected = NullPointerException.class)
	public void testPersistNull() {
		runner.persistNull(em, context());
	}

	@Test
	public void testPersistRollback() {
		runner.persistRollback(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistRollbackOnly() {
		runner.persistRollbackOnly(em, context());
	}

	@Test
	public void testPersistCascade() {
		runner.persistCascade(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistTwiceInOne() {
		runner.persistTwice(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistWithoutCascade() {
		runner.persistWithoutCascade(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistDetached() {
		runner.persistDetachedEntity(em, context());
	}

	@Test
	public void testPersistSimpleList() {
		runner.persistSimpleList(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistSimpleListNoCascade() {
		runner.persistSimpleListNoCascade(em, context());
	}

	@Test
	public void testPersistReferencedList() {
		runner.persistReferencedList(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistReferencedListNoCascade() {
		runner.persistReferencedListNoCascade(em, context());
	}

	@Test
	public void testPersistSimpleAndReferencedList() {
		runner.persistSimpleAndReferencedList(em, context());
	}

	@Test
	public void testPersistProperties() {
		runner.persistProperties(em, context());
	}

	@Test
	public void testPersistPropertiesEmpty() {
		runner.persistPropertiesEmpty(em, context());
	}

	private URI context() {
		// OWLAPI doesn't use contexts
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
