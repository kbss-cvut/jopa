package cz.cvut.kbss.jopa.test.query.runner;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import static org.junit.Assert.*;

public abstract class TypedQueryRunner extends BaseQueryRunner {

    protected TypedQueryRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testFindAll() {
        logger.config("Test: select all entities of a certain type.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassD> .}";
        final List<OWLClassD> ds = new ArrayList<>();
        final TypedQuery<OWLClassD> q = getEntityManager().createNativeQuery(query, OWLClassD.class);
        ds.addAll(QueryTestEnvironment.getDataByContext(null, OWLClassD.class));
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

    @Test
    public void testSelectByTypeAndDataPropertyValue() {
        logger.config("Test: select entity by its type and data property value.");
        final OWLClassB b = QueryTestEnvironment.getData(OWLClassB.class).get(5);
        final String query =
                "SELECT ?x WHERE { " +
                        "?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassB> ; " +
                        "<http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute> \""
                        + b.getStringAttribute() + "\" . }";
        final TypedQuery<OWLClassB> q = getEntityManager().createNativeQuery(query, OWLClassB.class);
        final OWLClassB res = q.getSingleResult();
        assertNotNull(res);
        assertEquals(b.getUri(), res.getUri());
        assertEquals(b.getStringAttribute(), res.getStringAttribute());
    }

    @Test
    public void testSelectByObjectProperty() {
        logger.config("Test: select entity by object property value.");
        final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> ?y . }";
        final TypedQuery<OWLClassD> q = getEntityManager().createNativeQuery(query, OWLClassD.class);
        final List<OWLClassD> ds = new ArrayList<>();
        ds.addAll(QueryTestEnvironment.getDataByContext(null, OWLClassD.class));
        final int cnt = ds.size() / 2;
        assertTrue(cnt > 1);
        final List<OWLClassD> res = q.setMaxResults(cnt).getResultList();
        assertEquals(cnt, res.size());
    }

    @Test
    public void testSetMaxResults() {
        logger.config("Test: set maximum number of results.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
        final TypedQuery<OWLClassE> q = getEntityManager().createNativeQuery(query, OWLClassE.class);
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

    @Test(expected = IllegalArgumentException.class)
    public void testSetMaxResultsNegative() {
        logger.config("Test: set maximum number of results. Negative argument.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
        final Query<OWLClassE> q = getEntityManager().createNativeQuery(query, OWLClassE.class);
        q.setMaxResults(-1);
    }

    @Test
    public void testSetMaxResultsZero() {
        logger.config("Test: set maximum number of results. Zero argument.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
        final Query<OWLClassE> q = getEntityManager().createNativeQuery(query, OWLClassE.class);
        q.setMaxResults(0);
        final List<OWLClassE> res = q.getResultList();
        assertNotNull(res);
        assertTrue(res.isEmpty());
    }

    @Test
    public void testGetSingleResult() {
        logger.config("Test: get single result.");
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String query =
                "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> \""
                        + a.getStringAttribute() + "\" .}";
        final Query<OWLClassA> q = getEntityManager().createNativeQuery(query, OWLClassA.class);
        final OWLClassA res = q.getSingleResult();
        assertNotNull(res);
        assertEquals(a.getUri(), res.getUri());
    }

    @Test(expected = NoUniqueResultException.class)
    public void testGetSingleResultMultiples() {
        logger.config("Test: get single result. No unique result.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
        final Query<OWLClassE> q = getEntityManager().createNativeQuery(query, OWLClassE.class);
        q.getSingleResult();
    }

    @Test(expected = NoResultException.class)
    public void testGetSingleResultNoResult() {
        logger.config("Test: get single result. No result.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX> . }";
        final Query<OWLClassE> q = getEntityManager().createNativeQuery(query, OWLClassE.class);
        q.getSingleResult();
    }

    @Test(expected = NullPointerException.class)
    public void testCreateQueryNullQuery() {
        logger.config("Test: create query. Null query passed.");
        getEntityManager().createNativeQuery(null, OWLClassA.class);
    }

    @Test(expected = NullPointerException.class)
    public void testCreateQueryNullClass() {
        logger.config("Test: create query. Null result class passed.");
        final String query = "SELECT ?x WHERE { ?x ?y ?z .}";
        getEntityManager().createNativeQuery(query, (Class<OWLClassA>) null);
    }

    @Test
    public void askQueryReturnsTrue() {
        logger.config("Test: execute a ASK query which returns true.");
        final String query = "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";
        final Query<Boolean> q = getEntityManager().createNativeQuery(query, Boolean.class);
        final Boolean res = q.getSingleResult();
        assertNotNull(res);
        assertTrue(res);
    }

    @Test
    public void askQueryReturnsFalse() {
        logger.config("Test: execute a ASK query which returns false.");
        final String query = "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX> . }";
        final Query<Boolean> q = getEntityManager().createNativeQuery(query, Boolean.class);
        final List<Boolean> res = q.getResultList();
        assertNotNull(res);
        assertEquals(1, res.size());
        assertFalse(res.get(0));
    }
}
