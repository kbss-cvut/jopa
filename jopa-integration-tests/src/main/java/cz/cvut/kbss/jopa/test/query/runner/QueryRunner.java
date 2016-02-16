/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.query.runner;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import org.junit.Test;

import java.net.URI;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public abstract class QueryRunner extends BaseQueryRunner {

    protected QueryRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testSelectByType() {
        logger.config("Test: get URIs of individuals of a certain type.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#TypeA> . }";
        final Query q = getEntityManager().createNativeQuery(query);
        final List<OWLClassA> as = QueryTestEnvironment.getDataByContext(null, OWLClassA.class);
        assertNotNull(q);

        final List res = q.getResultList();

        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(as.size(), res.size());
        boolean found;
        for (OWLClassA a : as) {
            found = false;
            for (Object row : res) {
                if (a.getUri().equals(row)) {
                    found = true;
                    break;
                }
            }
            assertTrue(found);
        }
    }

    @Test
    public void testSelectByDataProperty() {
        logger.config("Test: select data property values.");
        final String query = "SELECT ?y WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#B-stringAttribute> ?y . }";
        final Query q = getEntityManager().createNativeQuery(query);
        final Set<String> exp = QueryTestEnvironment.getDataByContext(null, OWLClassB.class).stream()
                                                    .map(OWLClassB::getStringAttribute).collect(Collectors.toSet());

        final List res = q.getResultList();

        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(exp.size(), res.size());
        for (Object lst2 : res) {
            assertTrue(lst2 instanceof String);
            // False means we got the expected value
            assertFalse(exp.add((String) lst2));
        }
    }

    @Test
    public void testSelectByObjectProperty() {
        logger.config("Test: select object property values.");
        final OWLClassD d = QueryTestEnvironment.getData(OWLClassD.class).get(0);
        final String query = "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA> ?y . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setParameter("y", d.getOwlClassA().getUri());

        final List res = q.getResultList();

        assertEquals(1, res.size());
        final Object subRes = res.get(0);
        assertEquals(d.getUri(), subRes);
    }

    @Test
    public void testSelectTypes() {
        logger.config("Test: select individual's types.");
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final Set<String> types = a.getTypes();
        types.add(a.getClass().getAnnotation(OWLClass.class).iri());
        final String query = "SELECT ?x WHERE { ?instance a ?x . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setParameter("instance", a.getUri());

        final List res = q.getResultList();
        // The result can contain more types (inference)
        assertTrue(res.size() >= types.size());
        for (String type : types) {
            assertTrue(res.contains(URI.create(type)));
        }
    }

    @Test
    public void testSetMaxResults() {
        logger.config("Test: set maximum number of results.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
        final Query q = getEntityManager().createNativeQuery(query);
        final int max = 5;
        assertTrue(max < QueryTestEnvironment.getData(OWLClassE.class).size());
        assertEquals(Integer.MAX_VALUE, q.getMaxResults());
        q.setMaxResults(max);
        assertEquals(max, q.getMaxResults());

        final List res = q.getResultList();
        assertNotNull(res);
        assertFalse(res.isEmpty());
        assertEquals(max, res.size());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testSetMaxResultsNegative() {
        logger.config("Test: set maximum number of results. Negative argument.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setMaxResults(-1);
    }

    @Test
    public void testSetMaxResultsZero() {
        logger.config("Test: set maximum number of results. Zero argument.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setMaxResults(0);

        final List res = q.getResultList();
        assertNotNull(res);
        assertTrue(res.isEmpty());
    }

    @Test
    public void testGetSingleResult() {
        logger.config("Test: get single result.");
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String query =
                "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> ?y .}";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setParameter("y", a.getStringAttribute(), "en");

        final Object res = q.getSingleResult();
        assertNotNull(res);
        assertEquals(a.getUri(), res);
    }

    @Test(expected = NoUniqueResultException.class)
    public void testGetSingleResultMultiples() {
        logger.config("Test: get single result. No unique result.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.getSingleResult();
    }

    @Test(expected = NoResultException.class)
    public void testGetSingleResultNoResult() {
        logger.config("Test: get single result. No result.");
        final String query = "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX> . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.getSingleResult();
    }

    @Test
    public void testSelectQueryWithPositionalParameters() {
        logger.config("Test: select query using positional parameters.");
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String query =
                "SELECT ?x WHERE { ?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute> $ .}";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setParameter(1, a.getStringAttribute(), "en");

        final Object res = q.getSingleResult();
        assertNotNull(res);
        assertEquals(a.getUri(), res);
    }

    @Test
    public void testSelectWithOptionalReturnsNullInUnfilledColumns() throws Exception {
        logger.config("Test: select query with optional. The result should have nulls in places of empty values.");
        final String query =
                "SELECT ?x ?s WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> ." +
                        " OPTIONAL {?x <http://krizik.felk.cvut.cz/ontologies/jopa/attributes#E-stringAttribute> ?s . } }";
        final OWLClassE e = new OWLClassE();
        final EntityManager em = getEntityManager();
        em.getTransaction().begin();
        em.persist(e);
        em.getTransaction().commit();
        final Query q = em.createNativeQuery(query);

        final List result = q.getResultList();
        assertFalse(result.isEmpty());
        for (Object row : result) {
            final Object[] rowArr = (Object[]) row;
            if (rowArr[0].equals(e.getUri().toString())) {
                assertNull(rowArr[1]);
            }
        }
    }
}
