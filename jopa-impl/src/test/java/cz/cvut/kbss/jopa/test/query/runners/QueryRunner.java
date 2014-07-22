package cz.cvut.kbss.jopa.test.query.runners;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;

public class QueryRunner {

	private final Logger logger;

	public QueryRunner(Logger logger) {
		this.logger = logger;
	}

	public void selectByType(EntityManager em, URI... ctxs) {
		logger.config("Test: get URIs of individuals of a certain type.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#TypeA> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
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

	public void selectByDataProperty(EntityManager em, URI... ctxs) {
		logger.config("Test: select data property values.");
		final String query = "SELECT ?y WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute> ?y . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		final List<List<String>> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		final Set<String> exp = new HashSet<>();
		for (OWLClassB e : QueryTestEnvironment.getData(OWLClassB.class)) {
			exp.add(e.getStringAttribute());
		}
		assertEquals(exp.size(), res.size());
		for (List<String> lst2 : res) {
			assertEquals(1, lst2.size());
			// False means we got the expected value
			assertFalse(exp.add(lst2.get(0)));
		}
	}

	public void selectByObjectProperty(EntityManager em, URI... ctxs) {
		logger.config("Test: select object property values.");
		final OWLClassD d = QueryTestEnvironment.getData(OWLClassD.class).get(0);
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> <"
				+ d.getOwlClassA().getUri().toString() + "> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		final List<List<String>> res = q.getResultList();
		assertEquals(1, res.size());
		final List<String> subRes = res.get(0);
		assertEquals(1, subRes.size());
		assertEquals(d.getUri().toString(), subRes.get(0));
	}

	public void selectTypes(EntityManager em, URI... ctxs) {
		logger.config("Test: select individual's types.");
		final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
		final Set<String> types = a.getTypes();
		types.add(a.getClass().getAnnotation(OWLClass.class).iri());
		final String query = "SELECT ?x WHERE { <" + a.getUri().toString() + "> a ?x . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		final List<List<String>> res = q.getResultList();
		// The result can contain more types (inference)
		assertTrue(res.size() >= types.size());
		boolean found = false;
		for (String type : types) {
			found = false;
			for (List<String> lst : res) {
				assertEquals(1, lst.size());
				if (type.equals(lst.get(0))) {
					found = true;
					break;
				}
			}
			assertTrue(found);
		}
	}

	public void executeSetMaxResults(EntityManager em, URI... ctxs) {
		logger.config("Test: set maximum number of results.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
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

	public void executeSetMaxResultsNegative(EntityManager em, URI... ctxs) {
		logger.config("Test: set maximum number of results. Negative argument.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		q.setMaxResults(-1);
		fail("This line should not have been reached.");
	}

	public void executeSetMaxResultsZero(EntityManager em, URI... ctxs) {
		logger.config("Test: set maximum number of results. Zero argument.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		q.setMaxResults(0);
		final List<List<String>> res = q.getResultList();
		assertNotNull(res);
		assertTrue(res.isEmpty());
	}

	public void getSingleResult(EntityManager em, URI... ctxs) {
		logger.config("Test: get single result.");
		final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> \""
				+ a.getStringAttribute() + "\" .}";
		final Query<List<String>> q = em.createNativeQuery(query);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		final List<String> res = q.getSingleResult();
		assertNotNull(res);
		assertEquals(1, res.size());
		assertEquals(a.getUri().toString(), res.get(0));
	}

	public void getSingleResultMultiple(EntityManager em, URI... ctxs) {
		logger.config("Test: get single result. No unique result.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		List<String> res = q.getSingleResult();
		fail("This line should not have been reached.");
		assertNotNull(res);
	}

	public void getSingleResultNoResult(EntityManager em, URI... ctxs) {
		logger.config("Test: get single result. No result.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX> . }";
		final Query<List<String>> q = em.createNativeQuery(query);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		final List<String> res = q.getSingleResult();
		fail("This line should not have been reached.");
		assertNotNull(res);
	}
}
