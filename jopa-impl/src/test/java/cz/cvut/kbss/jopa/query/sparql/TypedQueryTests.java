package cz.cvut.kbss.jopa.query.sparql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassC;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.OWLClassE;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.jopa.query.env.QueryTestEnvironment;

/**
 * Tests of the TypedQuery implementation.
 * 
 * @author ledvima1
 * 
 */
public class TypedQueryTests {

	private static final Logger LOG = Logger.getLogger(TypedQueryTests.class.getName());

	private static EntityManagerFactory emf;

	private EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		final EntityManager em = TestEnvironment.getPersistenceConnector("SPARQLTypedQueryTests");
		QueryTestEnvironment.generateTestData(em);
		emf = em.getEntityManagerFactory();
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
	public void testCreateQuery() {
		LOG.config("Test: create query instance.");
		final String query = "SELECT ?x WHERE { ?x ?y ?z .}";
		final TypedQuery<OWLClassA> q = em.createNativeQuery(query, OWLClassA.class);
		assertNotNull(q);
		assertEquals(Integer.MAX_VALUE, q.getMaxResults());
	}

	@Test(expected = NullPointerException.class)
	public void testCreateQueryNullQuery() {
		LOG.config("Test: create query. Null query passed.");
		@SuppressWarnings("unused")
		final TypedQuery<OWLClassA> q = em.createNativeQuery(null, OWLClassA.class);
		fail("This line should not have been reached.");
	}

	@Test(expected = NullPointerException.class)
	public void testCreateQueryNullClass() {
		LOG.config("Test: create query. Null query passed.");
		final String query = "SELECT ?x WHERE { ?x ?y ?z .}";
		@SuppressWarnings("unused")
		final TypedQuery<OWLClassA> q = em.createNativeQuery(query, (Class<OWLClassA>) null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testSelectByType() {
		LOG.config("Test: select all instances of class OWLClassE.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final TypedQuery<OWLClassE> q = em.createNativeQuery(query, OWLClassE.class);
		final List<OWLClassE> es = QueryTestEnvironment.getData(OWLClassE.class);
		final List<OWLClassE> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		assertEquals(es.size(), res.size());
		boolean found = false;
		for (OWLClassE e : es) {
			found = false;
			for (OWLClassE ee : res) {
				if (e.getUri().equals(ee.getUri())) {
					assertEquals(e.getStringAttribute(), ee.getStringAttribute());
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
	}

	@Test
	public void testSelectByDataProperty() {
		LOG.config("Test: select entities by data property.");
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute> ?y . }";
		final TypedQuery<OWLClassB> q = em.createNativeQuery(query, OWLClassB.class);
		final List<OWLClassB> bs = QueryTestEnvironment.getData(OWLClassB.class);
		final List<OWLClassB> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		assertEquals(bs.size(), res.size());
		boolean found = false;
		for (OWLClassB b : bs) {
			found = false;
			for (OWLClassB bb : res) {
				if (b.getUri().equals(bb.getUri())) {
					assertEquals(b.getStringAttribute(), bb.getStringAttribute());
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
	}

	@Test
	public void testSelectByObjectPropertyValue() {
		LOG.config("Test: select entities by object property values.");
		final List<OWLClassD> ds = QueryTestEnvironment.getData(OWLClassD.class);
		final OWLClassD dOne = ds.get(0);
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> <"
				+ dOne.getOwlClassA().getUri().toString() + "> . }";
		final TypedQuery<OWLClassD> q = em.createNativeQuery(query, OWLClassD.class);
		final List<OWLClassD> res = q.getResultList();
		assertNotNull(res);
		assertEquals(1, res.size());
		assertEquals(dOne.getUri(), res.get(0).getUri());
		assertNotNull(res.get(0).getOwlClassA());
		assertEquals(dOne.getOwlClassA().getUri(), res.get(0).getOwlClassA().getUri());
	}

	@Test
	public void testGetSingleResult() {
		LOG.config("Test: get single result.");
		final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(1);
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> \""
				+ a.getStringAttribute() + "\" .}";
		final TypedQuery<OWLClassA> q = em.createNativeQuery(query, OWLClassA.class);
		final OWLClassA res = q.getSingleResult();
		assertNotNull(res);
		assertEquals(a.getUri(), res.getUri());
		assertEquals(a.getStringAttribute(), res.getStringAttribute());
	}

	@Test(expected = NoUniqueResultException.class)
	public void testGetSingleResultNoUnique() {
		LOG.config("Test: get single result. No unique result.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassC> . }";
		final TypedQuery<OWLClassC> q = em.createNativeQuery(query, OWLClassC.class);
		@SuppressWarnings("unused")
		final OWLClassC res = q.getSingleResult();
		fail("This line should not have been reached.");
	}

	@Test(expected = NoResultException.class)
	public void testGetSingleResultNoResult() {
		LOG.config("Test: get single result. No result.");
		final String query = "SELECT ?x WHERE {?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> \"unknown\" . }";
		final TypedQuery<OWLClassA> q = em.createNativeQuery(query, OWLClassA.class);
		@SuppressWarnings("unused")
		final OWLClassA res = q.getSingleResult();
		fail("This line should not have been reached.");
	}

	@Test
	public void testSetMaxResults() {
		LOG.config("Test: set max results.");
		final String query = "SELECT ?x WHERE {?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> ?y . }";
		final List<OWLClassA> as = QueryTestEnvironment.getData(OWLClassA.class);
		final int count = 5;
		assertTrue(count < as.size());
		final TypedQuery<OWLClassA> q = em.createNativeQuery(query, OWLClassA.class);
		assertEquals(Integer.MAX_VALUE, q.getMaxResults());
		q.setMaxResults(count);
		assertEquals(count, q.getMaxResults());
		final List<OWLClassA> res = q.getResultList();
		assertTrue(res.size() <= count);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetMaxResultsInvalid() {
		LOG.config("Test: set max results. Invalid value.");
		final String query = "SELECT ?x WHERE {?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> ?y . }";
		final TypedQuery<OWLClassA> q = em.createNativeQuery(query, OWLClassA.class);
		q.setMaxResults(-10);
		fail("This line should not have been reached.");
	}
}
