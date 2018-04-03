/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import org.junit.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public abstract class QueryRunner extends BaseQueryRunner {

    protected QueryRunner(Logger logger) {
        super(logger);
    }

    @Test
    public void testSelectByType() {
        logger.debug("Test: get URIs of individuals of a certain type.");
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
        logger.debug("Test: select data property values.");
        final String query =
                "SELECT ?y WHERE { ?x ?stringAtt ?y . }";
        final Query q = getEntityManager().createNativeQuery(query)
                                          .setParameter("stringAtt", URI.create(Vocabulary.P_B_STRING_ATTRIBUTE));
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
        logger.debug("Test: select object property values.");
        final OWLClassD d = QueryTestEnvironment.getData(OWLClassD.class).get(0);
        final String query = "SELECT ?x WHERE { ?x a ?type ; ?hasA ?y . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_D))
         .setParameter("hasA", URI.create(Vocabulary.P_HAS_OWL_CLASS_A)).setParameter("y", d.getOwlClassA().getUri());

        final List res = q.getResultList();

        assertEquals(1, res.size());
        final Object subRes = res.get(0);
        assertEquals(d.getUri(), subRes);
    }

    @Test
    public void testSelectTypes() {
        logger.debug("Test: select individual's types.");
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
        logger.debug("Test: set maximum number of results.");
        final String query =
                "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
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
        logger.debug("Test: set maximum number of results. Negative argument.");
        final String query =
                "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setMaxResults(-1);
    }

    @Test
    public void testSetMaxResultsZero() {
        logger.debug("Test: set maximum number of results. Zero argument.");
        final String query =
                "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setMaxResults(0);

        final List res = q.getResultList();
        assertNotNull(res);
        assertTrue(res.isEmpty());
    }

    @Test
    public void testGetSingleResult() {
        logger.debug("Test: get single result.");
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
        logger.debug("Test: get single result. No unique result.");
        final String query =
                "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassE> . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.getSingleResult();
    }

    @Test(expected = NoResultException.class)
    public void testGetSingleResultNoResult() {
        logger.debug("Test: get single result. No result.");
        final String query =
                "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX> . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.getSingleResult();
    }

    @Test
    public void testSelectQueryWithPositionalParameters() {
        logger.debug("Test: select query using positional parameters.");
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
    public void selectWithOptionalReturnsNullInUnfilledColumns() {
        logger.debug("Test: select query with optional. The result should have nulls in places of empty values.");
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

    @Test
    public void testCreateNamedNativeQueryWithParameterSetting() {
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final List res = getEntityManager().createNamedQuery("OWLClassA.findByString")
                                           .setParameter("str", a.getStringAttribute(), "en").getResultList();
        assertFalse(res.isEmpty());
        assertTrue(res.contains(a.getUri()));
    }

    @Test
    public void queryWithMappingReturnsResultWithVariablesMappedAccordingly() {
        final List res = getEntityManager().createNativeQuery("SELECT * WHERE {" +
                "?x a <" + Vocabulary.C_OWL_CLASS_A + "> ;" +
                "<" + Vocabulary.P_A_STRING_ATTRIBUTE + "> ?y ." +
                "}", OWLClassA.VARIABLE_MAPPING).getResultList();
        final Map<String, String> expected = new HashMap<>();
        QueryTestEnvironment.getData(OWLClassA.class)
                            .forEach(a -> expected.put(a.getUri().toString(), a.getStringAttribute()));
        assertEquals(expected.size(), res.size());
        for (Object row : res) {
            assertTrue(row instanceof Object[]);
            final Object[] elems = (Object[]) row;
            assertEquals(2, elems.length);
            assertTrue(expected.containsKey(elems[0]));
            assertEquals(expected.get(elems[0]), elems[1]);
        }
    }

    @Test
    public void queryWithConstructorMappingReturnsCorrectInstances() {
        final List res = getEntityManager().createNativeQuery("SELECT * WHERE {" +
                "?x a <" + Vocabulary.C_OWL_CLASS_A + "> ;" +
                "<" + Vocabulary.P_A_STRING_ATTRIBUTE + "> ?y ." +
                "}", OWLClassA.CONSTRUCTOR_MAPPING).getResultList();
        final Map<URI, OWLClassA> expected = new HashMap<>();
        QueryTestEnvironment.getData(OWLClassA.class).forEach(a -> expected.put(a.getUri(), a));

        verifyOWLClassAInstances(res, expected);
    }

    private void verifyOWLClassAInstances(List res, Map<URI, OWLClassA> expected) {
        for (Object item : res) {
            assertTrue(item instanceof OWLClassA);
            final OWLClassA a = (OWLClassA) item;
            assertTrue(expected.containsKey(a.getUri()));
            assertEquals(expected.get(a.getUri()).getStringAttribute(), a.getStringAttribute());
        }
    }

    @Test
    public void queryWithEntityMappingReturnsCorrectManagedInstances() {
        final List res = getEntityManager().createNativeQuery("SELECT * WHERE {" +
                "?x a <" + Vocabulary.C_OWL_CLASS_A + "> ;" +
                "<" + Vocabulary.P_A_STRING_ATTRIBUTE + "> ?stringAttribute ." +
                "}", OWLClassA.ENTITY_MAPPING).getResultList();
        final Map<URI, OWLClassA> expected = new HashMap<>();
        QueryTestEnvironment.getData(OWLClassA.class).forEach(a -> expected.put(a.getUri(), a));

        verifyOWLClassAInstances(res, expected);
        for (Object item : res) {
            assertTrue(getEntityManager().contains(item));
        }
    }

    @Test
    public void queryWithEntityMappingLoadsReferencedEntitiesAsWell() {
        final List res = getEntityManager().createNativeQuery("SELECT ?x ?y WHERE {" +
                "?x a ?dType ;" +
                "?hasA ?y . }", OWLClassD.MAPPING_NAME)
                                           .setParameter("dType", URI.create(Vocabulary.C_OWL_CLASS_D))
                                           .setParameter("hasA", URI.create(Vocabulary.P_HAS_OWL_CLASS_A))
                                           .getResultList();
        final Map<URI, OWLClassD> expected = new HashMap<>();
        QueryTestEnvironment.getData(OWLClassD.class).forEach(d -> expected.put(d.getUri(), d));

        assertEquals(expected.size(), res.size());
        for (Object row : res) {
            assertTrue(row instanceof OWLClassD);
            final OWLClassD inst = (OWLClassD) row;
            assertTrue(expected.containsKey(inst.getUri()));
            assertNotNull(inst.getOwlClassA());
            final OWLClassA expectedA = expected.get(inst.getUri()).getOwlClassA();
            verifyOwlClassAInstance(expectedA, inst.getOwlClassA());
            assertTrue(getEntityManager().contains(inst));
            assertTrue(getEntityManager().contains(inst.getOwlClassA()));
        }
    }

    private void verifyOwlClassAInstance(OWLClassA expected, OWLClassA actual) {
        if (expected == null) {
            assertNull(actual);
            return;
        }
        assertNotNull(actual);
        assertEquals(expected.getUri(), actual.getUri());
        assertEquals(expected.getStringAttribute(), actual.getStringAttribute());
        assertEquals(expected.getTypes(), actual.getTypes());
    }

    @Test
    public void queryWithEntityMappingLoadsReferencedEntityAndInheritedAttributes() {
        final List res = getEntityManager().createNativeQuery("SELECT * WHERE {" +
                "?x a ?type ;" +
                "?hasA ?y ;" +
                "?rdfsLabel ?label ;" +
                "?hasDescription ?description ;" +
                "?hasInt ?intAttribute ." +
                "}", OWLClassT.MAPPING_NAME)
                                           .setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_T))
                                           .setParameter("hasA", URI.create(Vocabulary.P_HAS_OWL_CLASS_A))
                                           .setParameter("rdfsLabel", URI.create(CommonVocabulary.RDFS_LABEL))
                                           .setParameter("hasDescription", URI.create(CommonVocabulary.DC_DESCRIPTION))
                                           .setParameter("hasInt", URI.create(Vocabulary.P_T_INTEGER_ATTRIBUTE))
                                           .getResultList();
        final Map<URI, OWLClassT> expected = new HashMap<>();
        QueryTestEnvironment.getData(OWLClassT.class).forEach(t -> expected.put(t.getUri(), t));

        assertEquals(expected.size(), res.size());
        for (Object row : res) {
            assertTrue(row instanceof OWLClassT);
            final OWLClassT tActual = (OWLClassT) row;
            assertTrue(expected.containsKey(tActual.getUri()));
            final OWLClassT tExpected = expected.get(tActual.getUri());
            assertEquals(tExpected.getName(), tActual.getName());
            assertEquals(tExpected.getDescription(), tActual.getDescription());
            assertEquals(tExpected.getIntAttribute(), tActual.getIntAttribute());
            verifyOwlClassAInstance(tExpected.getOwlClassA(), tActual.getOwlClassA());
        }
    }

    @Test
    public void executeUpdateRunsUpdateOnRepository() {
        final EntityManager em = getEntityManager();
        final OWLClassA instance = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String newValue = "UpdatedValue";
        final String update = "DELETE { ?inst ?property ?origValue . }" +
                "INSERT { ?inst ?property ?newValue . } WHERE {" +
                "?inst ?property ?origValue . }";
        em.createNativeQuery(update).setParameter("inst", instance.getUri()).setParameter("property", URI.create(
                Vocabulary.P_A_STRING_ATTRIBUTE)).setParameter("newValue", newValue, "en").executeUpdate();

        final OWLClassA result = em.find(OWLClassA.class, instance.getUri());
        assertEquals(newValue, result.getStringAttribute());
    }

    @Test
    public void executeUpdateRunsDeleteOnRepository() {
        final EntityManager em = getEntityManager();
        final OWLClassA instance = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        assertNotNull(instance.getStringAttribute());
        final String update = "DELETE { ?inst ?property ?origValue . } WHERE {" +
                "?inst ?property ?origValue . }";
        em.createNativeQuery(update).setParameter("inst", instance.getUri())
          .setParameter("property", URI.create(Vocabulary.P_A_STRING_ATTRIBUTE)).executeUpdate();

        final OWLClassA result = em.find(OWLClassA.class, instance.getUri());
        assertNull(result.getStringAttribute());
    }

    @Test
    public void executeUpdateRunsInsertOnRepository() {
        final EntityManager em = getEntityManager();
        final URI newType = Generators.generateUri();
        final OWLClassA instance = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String update = "INSERT DATA { ?inst a ?newType . }";
        em.createNativeQuery(update).setParameter("inst", instance.getUri())
          .setParameter("newType", newType).executeUpdate();

        final OWLClassA result = em.find(OWLClassA.class, instance.getUri());
        assertTrue(result.getTypes().contains(newType.toString()));
    }

    @Test
    public void settingStringParameterEscapesTheParameterValue() {
        final EntityManager em = getEntityManager();
        final String query = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "SELECT ?x WHERE { ?x rdfs:comment ?comment . }";
        final String paramValue = "string\nWith\nNewlines";
        final List result = em.createNativeQuery(query).setParameter("comment", paramValue, "en").getResultList();
        assertTrue(result.isEmpty());   // The point here is that no exception is thrown and a result is returned
    }
}
