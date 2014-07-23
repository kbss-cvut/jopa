package cz.cvut.kbss.jopa.test.integration.jena;

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
import cz.cvut.kbss.jopa.test.utils.JenaTDBStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.exceptions.PrimaryKeyNotSetException;
import cz.cvut.kbss.ontodriver.impl.jena.DriverCachingJenaFactory;

public class TestCreateOperationsTDB {

	private static final Logger LOG = Logger.getLogger(TestCreateOperationsTDB.class.getName());

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
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistWithGenerated", storage, false,
				properties);
		runner.persistWithGenerated(em, context());
	}

	@Test(expected = PrimaryKeyNotSetException.class)
	public void testPersistWithoutId() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistWithoutId", storage, false,
				properties);
		runner.persistWithoutId(em, context());
	}

	@Test(expected = NullPointerException.class)
	public void testPersistNull() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistNull", storage, false,
				properties);
		runner.persistNull(em, context());
	}

	@Test
	public void testPersistRollback() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistRollback", storage, false,
				properties);
		runner.persistRollback(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistRollbackOnly() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistRollbackOnly", storage, false,
				properties);
		runner.persistRollbackOnly(em, context());
	}

	@Test
	public void testPersistCascade() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistWithCascade", storage, false,
				properties);
		runner.persistCascade(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistTwiceInOne() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistTwice", storage, false,
				properties);
		runner.persistTwice(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistWithoutCascade() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistWithoutCascade", storage,
				false, properties);
		runner.persistWithoutCascade(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistDetached() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistDetached", storage, false,
				properties);
		runner.persistDetachedEntity(em, context());
	}

	@Test
	public void testPersistSimpleList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistSimpleList", storage, false,
				properties);
		runner.persistSimpleList(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistSimpleListNoCascade() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistSimpleListNoCascade", storage,
				false, properties);
		runner.persistSimpleListNoCascade(em, context());
	}

	@Test
	public void testPersistReferencedList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistReferencedList", storage,
				false, properties);
		runner.persistReferencedList(em, context());
	}

	@Test(expected = RollbackException.class)
	public void testPersistReferencedListNoCascade() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistReferencedListNoCascade",
				storage, false, properties);
		runner.persistReferencedListNoCascade(em, context());
	}

	@Test
	public void testPersistSimpleAndReferencedList() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistSimpleAndReferencedList",
				storage, false, properties);
		runner.persistSimpleAndReferencedList(em, context());
	}

	@Test
	public void testPersistProperties() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistWithProperties", storage,
				false, properties);
		runner.persistProperties(em, context());
	}

	@Test
	public void testPersistPropertiesEmpty() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistWithPropertiesEmpty", storage,
				false, properties);
		runner.persistPropertiesEmpty(em, context());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void persistURITwiceInDifferentClasses() {
		em = TestEnvironment.getPersistenceConnector("JenaTDBPersistURITwiceInDifferentClasses",
				storage, false, properties);
		runner.persistURITwiceInDifferentClasses(em, context());
	}

	private URI context() {
		// OWLAPI doesn't use contexts
		return null;
	}

	private static StorageConfig initStorage() {
		return new JenaTDBStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.JENA_DRIVER_FACTORY, DriverCachingJenaFactory.class.getName());
		map.put(OWLAPIPersistenceProperties.LANG, "en");
		return map;
	}
}
