package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.integration.runners.CreateOperationsMultiContextRunner;
import cz.cvut.kbss.jopa.test.environment.SesameNativeStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class TestCreateOperationsMultiContextNativeStore {

	private static final Logger LOG = Logger
			.getLogger(TestCreateOperationsMultiContextNativeStore.class.getName());

	private static final StorageConfig storage = initStorage();
	private static final Map<String, String> properties = initProperties();

	private CreateOperationsMultiContextRunner runner;

	private EntityManager em;

	@Before
	public void setUp() {
		this.runner = new CreateOperationsMultiContextRunner(LOG);
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
	public void testPersistDataPropertyIntoContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiPersistDataPropertyIntoContext", storage, false, properties);
		runner.persistDataPropertyIntoContext(em);
	}

	@Test
	public void testPersistObjectPropertyIntoContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiPersistObjectPropertyIntoContext", storage, false, properties);
		runner.persistObjectPropertyIntoContext(em);
	}

	@Test
	public void testPersistWithGeneratedIntoContext() {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiPersistWithGeneratedIntoContext", storage, false, properties);
		runner.persistWithGeneratedIntoContext(em);
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistTwiceIntoOneContext() {
		em = TestEnvironment.getPersistenceConnector("SesameNativeMultiPersistTwiceIntoOneContext",
				storage, false, properties);
		runner.persistTwiceIntoOneContext(em);
	}

	@Test
	public void testPersistTwiceIntoDifferentContexts() {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiPersistTwiceIntoDifferentContexts", storage, false, properties);
		runner.persistTwiceIntoDifferentContexts(em);
	}

	@Test
	public void testPersistPropertiesIntoDifferentContext() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiPersistPropertiesIntoDifferentContext", storage, false,
				properties);
		runner.persistPropertiesIntoDifferent(em);
	}

	@Test
	public void testPersistCascadeIntoThreeContexts() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiPersistCascadeIntoThreeContexts", storage, false, properties);
		runner.persistCascadeIntoThreeContexts(em);
	}

	@Test
	public void testPersistSetWithAttributeContexts() throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativeMultiPersistSetWithAttributeContexts", storage, false, properties);
		runner.persistSetWithAttributeContexts(em);
	}

	@Test
	public void testPersistEntityWithObjectPropertyWithGeneratedIdentifierAndPutTheReferenceIntoContext()
			throws Exception {
		em = TestEnvironment.getPersistenceConnector(
				"SesameNativePersistEntityWithObjectPropertyWithGeneratedIdentifierContexts",
				storage, true, properties);
		runner.persistEntityWithObjectPropertyWithGeneratedIdentifierAndPutThePropertyIntoDifferentContext(em);
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
