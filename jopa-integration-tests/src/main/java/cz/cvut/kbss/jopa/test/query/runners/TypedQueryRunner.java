package cz.cvut.kbss.jopa.test.query.runners;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import static org.junit.Assert.*;

public class TypedQueryRunner {

	private final Logger logger;

	public TypedQueryRunner(Logger logger) {
		this.logger = logger;
	}

	public void findAll(EntityManager em, URI... ctxs) {
		logger.config("Test: select all entities of a certain type.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassD> .}";
		final List<OWLClassD> ds = new ArrayList<>();
		final TypedQuery<OWLClassD> q = em.createNativeQuery(query, OWLClassD.class);
		if (ctxs.length == 0) {
			ds.addAll(QueryTestEnvironment.getDataByContext(null, OWLClassD.class));
		}
		for (URI ctx : ctxs) {
			q.addContext(ctx);
			ds.addAll(QueryTestEnvironment.getDataByContext(ctx, OWLClassD.class));
		}
		final List<OWLClassD> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		assertEquals(ds.size(), res.size());
		boolean found;
		for (OWLClassD d : ds) {
			found = false;
			for (OWLClassD dd : res) {
				if (d.getUri().equals(dd.getUri())) {
					found = true;
					assertNotNull(dd.getOwlClassA());
					assertEquals(d.getOwlClassA().getUri(), dd.getOwlClassA().getUri());
					break;
				}
			}
			assertTrue(found);
		}
	}

	public void selectByTypeAndDataPropertyValue(EntityManager em, URI... ctxs) {
		logger.config("Test: select entity by its type and data property value.");
		final OWLClassB b = QueryTestEnvironment.getData(OWLClassB.class).get(5);
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassB> ; <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute> \""
				+ b.getStringAttribute() + "\" . }";
		final TypedQuery<OWLClassB> q = em.createNativeQuery(query, OWLClassB.class);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		final OWLClassB res = q.getSingleResult();
		assertNotNull(res);
		assertEquals(b.getUri(), res.getUri());
		assertEquals(b.getStringAttribute(), res.getStringAttribute());
	}

	public void selectByObjectProperty(EntityManager em, URI... ctxs) {
		logger.config("Test: select entity by object property value.");
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> ?y . }";
		final TypedQuery<OWLClassD> q = em.createNativeQuery(query, OWLClassD.class);
		final List<OWLClassD> ds = new ArrayList<>();
		if (ctxs.length == 0) {
			ds.addAll(QueryTestEnvironment.getDataByContext(null, OWLClassD.class));
		} else {
			for (URI ctx : ctxs) {
				q.addContext(ctx);
				ds.addAll(QueryTestEnvironment.getDataByContext(ctx, OWLClassD.class));
			}
		}
		final int cnt = ds.size() / 2;
		assertTrue(cnt > 1);
		final List<OWLClassD> res = q.setMaxResults(cnt).getResultList();
		assertEquals(cnt, res.size());
	}

	public void executeSetMaxResults(EntityManager em, URI... ctxs) {
		logger.config("Test: set maximum number of results.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final TypedQuery<OWLClassE> q = em.createNativeQuery(query, OWLClassE.class);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		final int max = 5;
		assertTrue(max < QueryTestEnvironment.getData(OWLClassE.class).size());
		assertEquals(Integer.MAX_VALUE, q.getMaxResults());
		q.setMaxResults(max);
		assertEquals(max, q.getMaxResults());
		final List<OWLClassE> res = q.getResultList();
		assertNotNull(res);
		assertFalse(res.isEmpty());
		assertEquals(max, res.size());
	}

	public void executeSetMaxResultsNegative(EntityManager em, URI... ctxs) {
		logger.config("Test: set maximum number of results. Negative argument.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<OWLClassE> q = em.createNativeQuery(query, OWLClassE.class);
		q.setMaxResults(-1);
		fail("This line should not have been reached.");
	}

	public void executeSetMaxResultsZero(EntityManager em, URI... ctxs) {
		logger.config("Test: set maximum number of results. Zero argument.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<OWLClassE> q = em.createNativeQuery(query, OWLClassE.class);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		q.setMaxResults(0);
		final List<OWLClassE> res = q.getResultList();
		assertNotNull(res);
		assertTrue(res.isEmpty());
	}

	public void getSingleResult(EntityManager em, URI... ctxs) {
		logger.config("Test: get single result.");
		final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
		final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> \""
				+ a.getStringAttribute() + "\" .}";
		final Query<OWLClassA> q = em.createNativeQuery(query, OWLClassA.class);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		final OWLClassA res = q.getSingleResult();
		assertNotNull(res);
		assertEquals(a.getUri(), res.getUri());
	}

	public void getSingleResultMultiple(EntityManager em, URI... ctxs) {
		logger.config("Test: get single result. No unique result.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
		final Query<OWLClassE> q = em.createNativeQuery(query, OWLClassE.class);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		final OWLClassE res = q.getSingleResult();
		fail("This line should not have been reached.");
		assertNotNull(res);
	}

	public void getSingleResultNoResult(EntityManager em, URI... ctxs) {
		logger.config("Test: get single result. No result.");
		final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX> . }";
		final Query<OWLClassE> q = em.createNativeQuery(query, OWLClassE.class);
		for (URI ctx : ctxs) {
			q.addContext(ctx);
		}
		final OWLClassE res = q.getSingleResult();
		fail("This line should not have been reached.");
		assertNotNull(res);
	}

	public void createQueryNullQuery(EntityManager em, URI... ctxs) {
		logger.config("Test: create query. Null query passed.");
		final TypedQuery<OWLClassA> q = em.createNativeQuery(null, OWLClassA.class);
		fail("This line should not have been reached.");
		assert q == null;
	}

	public void createQueryNullClass(EntityManager em, URI... ctxs) {
		logger.config("Test: create query. Null result class passed.");
		final String query = "SELECT ?x WHERE { ?x ?y ?z .}";
		final TypedQuery<OWLClassA> q = em.createNativeQuery(query, (Class<OWLClassA>) null);
		fail("This line should not have been reached.");
		assert q == null;
	}

	public void askQueryReturnsTrue(EntityManager em, URI... ctxs) {
        logger.config("Test: execute a ASK query which returns true.");
        final String query = "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";
        final Query<Boolean> q = em.createNativeQuery(query, Boolean.class);
        for (URI ctx : ctxs) {
            q.addContext(ctx);
        }
        final Boolean res = q.getSingleResult();
        assertNotNull(res);
        assertTrue(res);
    }

    public void askQueryReturnsFalse(EntityManager em, URI... ctxs) {
        logger.config("Test: execute a ASK query which returns false.");
        final String query = "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX> . }";
        final Query<Boolean> q = em.createNativeQuery(query, Boolean.class);
        for (URI ctx : ctxs) {
            q.addContext(ctx);
        }
        final List<Boolean> res = q.getResultList();
        assertNotNull(res);
        assertEquals(1, res.size());
        assertFalse(res.get(0));
    }
}
