package cz.cvut.kbss.jopa.test.query.sesame;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.net.URI;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runners.QueryRunner;
import cz.cvut.kbss.jopa.test.query.runners.TypedQueryRunner;
import cz.cvut.kbss.jopa.test.utils.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver_new.OntoDriverProperties;

// Ignored until proper context-aware query implementation is ready
@Ignore
public class SesameMemoryStoreMultiContextQueryTest {

	private static final Logger LOG = Logger.getLogger(SesameMemoryStoreMultiContextQueryTest.class
			.getName());

	private static URI CONTEXT_ONE = URI.create("http://krizik.felk.cvut.cz/ontologies/contextOne");
	private static URI CONTEXT_TWO = URI.create("http://krizik.felk.cvut.cz/ontologies/contextTwo");

	private static StorageConfig storage = initStorage();
	private static Map<String, String> properties = initProperties();

	private static EntityManager em;

	private QueryRunner runner;
	private TypedQueryRunner typedRunner;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SesameMultiContextSPARQLQueryTests", storage,
				false, properties);
		final Set<URI> contexts = new HashSet<>();
		contexts.add(CONTEXT_ONE);
		contexts.add(CONTEXT_TWO);
		QueryTestEnvironment.generateTestData(em, contexts);
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
	public void testSelectByTypeOneContext() {
		final OWLClassA a = QueryTestEnvironment.getDataByContext(CONTEXT_ONE, OWLClassA.class)
				.get(0);
		final OWLClassA aa = em
				.find(OWLClassA.class, a.getUri(), new EntityDescriptor(CONTEXT_ONE));
		assertNotNull(aa);
		final OWLClassA ac = QueryTestEnvironment.getDataByContext(CONTEXT_TWO, OWLClassA.class)
				.get(0);
		final OWLClassA aac = em.find(OWLClassA.class, ac.getUri(), new EntityDescriptor(
				CONTEXT_ONE));
		assertNull(aac);
		runner.selectByType(em, CONTEXT_ONE);
	}

	@Test
	public void testSelectByTypeAllContexts() {
		runner.selectByType(em, CONTEXT_ONE, CONTEXT_TWO);
	}

	@Test
	public void testSelectByDataProperty() {
		runner.selectByDataProperty(em, CONTEXT_TWO);
	}

	@Test
	public void testSelectByObjectProperty() {
		runner.selectByObjectProperty(em, CONTEXT_ONE);
	}

	@Test
	public void testSelectTypes() {
		runner.selectTypes(em, CONTEXT_ONE, CONTEXT_TWO);
	}

	@Test
	public void testSetMaxResults() {
		runner.executeSetMaxResults(em, CONTEXT_ONE);
	}

	@Test
	public void testSetMaxResultsZero() {
		runner.executeSetMaxResultsZero(em, CONTEXT_ONE, CONTEXT_TWO);
	}

	@Test
	public void testGetSingleResult() {
		runner.getSingleResult(em, CONTEXT_TWO);
	}

	@Test(expected = NoUniqueResultException.class)
	public void testGetSingleResultMultiples() {
		runner.getSingleResultMultiple(em, CONTEXT_ONE);
	}

	@Test(expected = NoResultException.class)
	public void testGetSingleResultNoResult() {
		runner.getSingleResultNoResult(em, CONTEXT_ONE);
	}

	@Test
	public void testFindAll_typed() {
		typedRunner.findAll(em, CONTEXT_TWO);
	}

	@Test
	public void testSelectByTypeAndDataPropertyValue_typed() {
		typedRunner.selectByTypeAndDataPropertyValue(em, CONTEXT_ONE);
	}

	@Test
	public void testSelectByObjectProperty_typed() {
		typedRunner.selectByObjectProperty(em, CONTEXT_TWO);
	}

	@Test
	public void testSetMaxResults_typed() {
		typedRunner.executeSetMaxResults(em, CONTEXT_ONE, CONTEXT_TWO);
	}

	@Test
	public void testGetSingleResult_typed() {
		typedRunner.getSingleResult(em, CONTEXT_ONE);
	}

	@Test(expected = NoUniqueResultException.class)
	public void testGetSingleResultMultiples_typed() {
		typedRunner.getSingleResultMultiple(em, CONTEXT_TWO);
	}

	@Test(expected = NoResultException.class)
	public void testGetSingleResultNoResult_typed() {
		typedRunner.getSingleResultNoResult(em, CONTEXT_ONE, CONTEXT_TWO);
	}

	@Test(expected = NullPointerException.class)
	public void testCreateQueryNullQuery_typed() {
		typedRunner.createQueryNullQuery(em, CONTEXT_ONE);
	}

	@Test(expected = NullPointerException.class)
	public void testCreateQueryNullClass_typed() {
		typedRunner.createQueryNullClass(em, CONTEXT_TWO);
	}

	private static StorageConfig initStorage() {
		return new SesameMemoryStorageConfig();
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> map = new HashMap<>();
		map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		map.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
		return map;
	}
}
