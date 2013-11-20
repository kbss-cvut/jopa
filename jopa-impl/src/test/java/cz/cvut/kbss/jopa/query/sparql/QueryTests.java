package cz.cvut.kbss.jopa.query.sparql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.owlapi.OWLClassE;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.jopa.query.env.QueryTestEnvironment;

public class QueryTests {

	private static final Logger LOG = Logger.getLogger(QueryTests.class.getName());

	private static Map<Class<?>, List<?>> data;
	private static EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		em = TestEnvironment.getPersistenceConnector("SPARQLQueryTests");
		data = QueryTestEnvironment.generateTestData(em);
		em.clear();
		em.getEntityManagerFactory().getCache().evictAll();
	}

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
		List<OWLClassE> es = (List<OWLClassE>) data.get(OWLClassE.class);
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
}
