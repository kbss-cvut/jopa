package cz.cvut.kbss.jopa.test.query.sparql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;
import java.util.logging.Logger;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.query.env.QueryTestEnvironment;

/**
 * Tests of the Query implementation.
 * 
 * @author ledvima1
 * 
 */
public class QueryTests {

	private static final Logger LOG = Logger.getLogger(QueryTests.class.getName());

	private static EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SPARQLQueryTests");
		QueryTestEnvironment.generateTestData(em);
		em.clear();
		em.getEntityManagerFactory().getCache().evictAll();
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		em.close();
		em.getEntityManagerFactory().close();
	}

	@Test
	public void testCreateQuery() {
		LOG.config("Test: create query.");
		final Query<List<String>> q = em.createNativeQuery("SELECT ?k WHERE blahblah");
		assertNotNull(q);
	}

	@Test(expected = NullPointerException.class)
	public void testCreateQueryNull() {
		LOG.config("Test: create query. Null passed as query.");
		final Query<List<String>> q = em.createNativeQuery(null);
		assertNull(q);
		fail("This line should not have been reached.");
	}

	@Test
	public void testSimpleSelect() throws Exception {
		LOG.config("Test: execute a simple select.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		final List<List<String>> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		final List<OWLClassE> es = QueryTestEnvironment.getData(OWLClassE.class);
		assertEquals(es.size(), res.size());
		boolean found = false;
		for (OWLClassE e : es) {
			found = false;
			for (List<String> l : res) {
				assertEquals(1, l.size());
				if (l.get(0).equals(e.getUri().toString())) {
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
	}

	@Test
	public void testSelectByPredicate() throws Exception {
		LOG.config("Test: select subjects having a predicate.");
		final String query = "SELECT ?x ?y WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> ?y . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		final List<List<String>> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		final List<OWLClassA> as = QueryTestEnvironment.getData(OWLClassA.class);
		assertEquals(as.size(), res.size());
		boolean found = false;
		for (OWLClassA a : as) {
			found = false;
			final String aUri = a.getUri().toString();
			for (List<String> ls : res) {
				assertEquals(2, ls.size());
				if (ls.get(0).equals(aUri)) {
					assertEquals(a.getStringAttribute(), ls.get(1));
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
	}

	@Test
	public void testSelectByObjectProperty() throws Exception {
		LOG.config("Test: select subject by object property.");
		final OWLClassD d = QueryTestEnvironment.getData(OWLClassD.class).get(0);
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> <"
				+ d.getOwlClassA().getUri().toString() + "> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		final List<List<String>> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		assertEquals(1, res.size());
		assertEquals(1, res.get(0).size());
		final String resUri = res.get(0).get(0);
		assertEquals(d.getUri().toString(), resUri);
	}

	@Test
	public void testSelectByMultipleTypes() throws Exception {
		LOG.config("Select URI and string attribute by types specified for entity OWLClassA.");
		final String query = "SELECT ?x ?y WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> ; "
				+ " a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#TypeA> . "
				+ "?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> ?y .}";
		final Query<List<String>> q = em.createNativeQuery(query);
		final List<List<String>> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		final List<OWLClassA> as = QueryTestEnvironment.getData(OWLClassA.class);
		boolean found = false;
		for (OWLClassA a : as) {
			found = false;
			final String aUri = a.getUri().toString();
			for (List<String> ls : res) {
				assertEquals(2, ls.size());
				if (ls.get(0).equals(aUri)) {
					assertEquals(a.getStringAttribute(), ls.get(1));
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
	}

	@Test
	public void testSetMaxResults() throws Exception {
		LOG.config("Test: set maximum number of results.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		final int max = 5;
		assertTrue(max < QueryTestEnvironment.getData(OWLClassE.class).size());
		assertEquals(Integer.MAX_VALUE, q.getMaxResults());
		q.setMaxResults(max);
		assertEquals(max, q.getMaxResults());
		final List<List<String>> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		assertEquals(max, res.size());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetMaxResultsNegative() throws Exception {
		LOG.config("Test: set maximum number of results. Negative argument.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		q.setMaxResults(-1);
		fail("This line should not have been reached.");
	}

	@Test
	public void testSetMaxResultsZero() throws Exception {
		LOG.config("Test: set maximum number of results. Zero argument.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		q.setMaxResults(0);
		final List<List<String>> res = q.getResultList();
		assertNotNull(res);
		assertTrue(res.isEmpty());
	}

	@Test
	public void testGetSingleResult() throws Exception {
		LOG.config("Test: get single result.");
		final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> \""
				+ a.getStringAttribute() + "\" .}";
		final Query<List<String>> q = em.createNativeQuery(query);
		final List<String> res = q.getSingleResult();
		assertNotNull(res);
		assertEquals(1, res.size());
		assertEquals(a.getUri().toString(), res.get(0));
	}

	@Test(expected = NoUniqueResultException.class)
	public void testGetSingleResultMultiple() throws Exception {
		LOG.config("Test: get single result. No unique result.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		List<String> res = q.getSingleResult();
		fail("This line should not have been reached.");
		assertNotNull(res);
	}

	@Test(expected = NoResultException.class)
	public void testGetSingleResultNoResult() throws Exception {
		LOG.config("Test: get single result. No result.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		final List<String> res = q.getSingleResult();
		fail("This line should not have been reached.");
		assertNotNull(res);
	}
}
