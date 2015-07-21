package cz.cvut.kbss.jopa.test.query.sesame;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runners.QueryRunner;
import cz.cvut.kbss.jopa.test.query.runners.TypedQueryRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class SesameMemStoreQueryTests {

	private static final Logger LOG = Logger.getLogger(SesameMemStoreQueryTests.class.getName());

	private static StorageConfig storage = initStorage();
	private static Map<String, String> properties = initProperties();

	private static EntityManager em;

	private QueryRunner runner;
	private TypedQueryRunner typedRunner;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameMemStoreSPARQLQueryTests", storage,
				false, properties);
		QueryTestEnvironment.generateTestData(em);
		em.clear();
		em.getEntityManagerFactory().getCache().evictAll();
	}

	@Before
	public void setupUp() throws Exception {
		this.runner = new QueryRunner(LOG);
		this.typedRunner = new TypedQueryRunner(LOG);
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		em.close();
		em.getEntityManagerFactory().close();
	}

	@Test
	public void testSelectByType() {
		runner.selectByType(em);
	}

	@Test
	public void testSelectByDataProperty() {
		runner.selectByDataProperty(em);
	}

	@Test
	public void testSelectByObjectProperty() {
		runner.selectByObjectProperty(em);
	}

	@Test
	public void testSelectTypes() {
		runner.selectTypes(em);
	}

	@Test
	public void testSetMaxResults() {
		runner.executeSetMaxResults(em);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetMaxResultsNegative() {
		runner.executeSetMaxResultsNegative(em);
	}

	@Test
	public void testSetMaxResultsZero() {
		runner.executeSetMaxResultsZero(em);
	}

	@Test
	public void testGetSingleResult() {
		runner.getSingleResult(em);
	}

	@Test(expected = NoUniqueResultException.class)
	public void testGetSingleResultMultiples() {
		runner.getSingleResultMultiple(em);
	}

	@Test(expected = NoResultException.class)
	public void testGetSingleResultNoResult() {
		runner.getSingleResultNoResult(em);
	}

	@Test
	public void testFindAll_typed() {
		typedRunner.findAll(em);
	}

	@Test
	public void testSelectByTypeAndDataPropertyValue_typed() {
		typedRunner.selectByTypeAndDataPropertyValue(em);
	}

	@Test
	public void testSelectByObjectProperty_typed() {
		typedRunner.selectByObjectProperty(em);
	}

	@Test
	public void testSetMaxResults_typed() {
		typedRunner.executeSetMaxResults(em);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetMaxResultsNegative_typed() {
		typedRunner.executeSetMaxResultsNegative(em);
	}

	@Test
	public void testSetMaxResultsZero_typed() {
		typedRunner.executeSetMaxResultsZero(em);
	}

	@Test
	public void testGetSingleResult_typed() {
		typedRunner.getSingleResult(em);
	}

	@Test(expected = NoUniqueResultException.class)
	public void testGetSingleResultMultiples_typed() {
		typedRunner.getSingleResultMultiple(em);
	}

	@Test(expected = NoResultException.class)
	public void testGetSingleResultNoResult_typed() {
		typedRunner.getSingleResultNoResult(em);
	}

	@Test(expected = NullPointerException.class)
	public void testCreateQueryNullQuery_typed() {
		typedRunner.createQueryNullQuery(em);
	}

	@Test(expected = NullPointerException.class)
	public void testCreateQueryNullClass_typed() {
		typedRunner.createQueryNullClass(em);
	}

	@Test
    public void typedAskQueryReturnsBoolean_true() {
        typedRunner.askQueryReturnsTrue(em);
    }

    @Test
    public void typedAskQueryReturnsBoolean_false() {
        typedRunner.askQueryReturnsFalse(em);
    }

	private static StorageConfig initStorage() {
		return new SesameMemoryStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
		map.put("storage", "new");
		return map;
	}
}
