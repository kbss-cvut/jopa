package cz.cvut.kbss.jopa.query.sparql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.jopa.owlapi.utils.StorageInfo;
import cz.cvut.kbss.jopa.query.env.QueryTestEnvironment;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public class MultiContextQueryTest {

	private static final Logger LOG = Logger.getLogger(MultiContextQueryTest.class.getName());

	private static final List<StorageInfo> storages = initStorages();

	private static EntityManagerFactory emf;
	private static List<Context> contexts;

	private EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		TestEnvironment.resetOwldbHibernateProvider();
		final EntityManager em = TestEnvironment.getPersistenceConnector(
				"SPARQLMultiContextQueryTests", storages, true);
		QueryTestEnvironment.generateTestData(em);
		emf = em.getEntityManagerFactory();
		contexts = em.getAvailableContexts();
		em.close();
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		emf.close();
	}

	@Before
	public void setUp() throws Exception {
		emf.getCache().evictAll();
		this.em = emf.createEntityManager();
	}

	@After
	public void tearDown() throws Exception {
		em.close();
	}

	@Test
	public void testSelectFromSingleContext() {
		LOG.config("Test: select subjects by type from a single context.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#TypeA> . }";
		final Query<List<String>> q = em.createNativeQuery(query, contexts.get(contexts.size() - 1)
				.getUri());
		assertNotNull(q);
		final List<List<String>> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		final List<OWLClassA> as = QueryTestEnvironment.getData(OWLClassA.class);
		assertEquals(as.size(), res.size());
		boolean found = false;
		for (OWLClassA a : as) {
			found = false;
			for (List<String> lst : res) {
				assertEquals(1, lst.size());
				if (a.getUri().toString().equals(lst.get(0))) {
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
	}

	// TODO Add tests

	private static List<StorageInfo> initStorages() {
		final List<StorageInfo> lst = new ArrayList<StorageInfo>(3);
		lst.add(new StorageInfo(OntologyConnectorType.OWLAPI, OwlapiStorageType.FILE));
		lst.add(new StorageInfo(OntologyConnectorType.OWLAPI, OwlapiStorageType.OWLDB));
		lst.add(new StorageInfo(OntologyConnectorType.JENA, OwlapiStorageType.FILE));
		return lst;
	}
}
