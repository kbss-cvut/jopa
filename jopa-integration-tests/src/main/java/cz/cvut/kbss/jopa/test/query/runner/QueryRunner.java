/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.test.query.runner;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.QueryHints;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassT;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.Quad;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.vocabulary.DC;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.hasItem;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public abstract class QueryRunner extends BaseQueryRunner {

    private static final String SELECT_E_BY_TYPE =
            "SELECT ?x WHERE { ?x a <" + Vocabulary.C_OWL_CLASS_E + "> . }";

    protected QueryRunner(Logger logger, DataAccessor dataAccessor) {
        super(logger, dataAccessor);
    }

    @Test
    void testSelectByType() {
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
    void testSelectByDataProperty() {
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
            assertInstanceOf(LangString.class, lst2);
            // False means we got the expected value
            assertFalse(exp.add(((LangString) lst2).getValue()));
        }
    }

    @Test
    void testSelectByObjectProperty() {
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
    void testSelectTypes() {
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final Set<String> types = a.getTypes();
        types.add(a.getClass().getAnnotation(OWLClass.class).iri());
        final String query = "SELECT ?x WHERE { ?instance a ?x . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setParameter("instance", a.getUri());

        final List res = q.getResultList();
        // The result can contain more types (inference)
        assertThat(res.size(), greaterThanOrEqualTo(types.size()));
        for (String type : types) {
            assertTrue(res.contains(URI.create(type)));
        }
    }

    @Test
    void testSetMaxResults() {
        final Query q = getEntityManager().createNativeQuery(SELECT_E_BY_TYPE);
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

    @Test
    void testSetMaxResultsNegative() {
        final Query q = getEntityManager().createNativeQuery(SELECT_E_BY_TYPE);
        assertThrows(IllegalArgumentException.class, () -> q.setMaxResults(-1));
    }

    @Test
    void testSetMaxResultsZero() {
        final Query q = getEntityManager().createNativeQuery(SELECT_E_BY_TYPE);
        q.setMaxResults(0);

        final List res = q.getResultList();
        assertNotNull(res);
        assertTrue(res.isEmpty());
    }

    @Test
    void testGetSingleResult() {
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String query =
                "SELECT ?x WHERE { ?x <" + Vocabulary.P_A_STRING_ATTRIBUTE + "> ?y .}";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setParameter("y", a.getStringAttribute(), "en");

        final Object res = q.getSingleResult();
        assertNotNull(res);
        assertEquals(a.getUri(), res);
    }

    @Test
    void testGetSingleResultMultiples() {
        final Query q = getEntityManager().createNativeQuery(SELECT_E_BY_TYPE);
        assertThrows(NoUniqueResultException.class, q::getSingleResult);
    }

    @Test
    void testGetSingleResultNoResult() {
        final String query =
                "SELECT ?x WHERE { ?x a <" + Vocabulary.CLASS_IRI_BASE + "OWLClassX> . }";
        final Query q = getEntityManager().createNativeQuery(query);
        assertThrows(NoResultException.class, q::getSingleResult);
    }

    @Test
    void testSelectQueryWithPositionalParameters() {
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String query =
                "SELECT ?x WHERE { ?x <" + Vocabulary.P_A_STRING_ATTRIBUTE + "> $ .}";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setParameter(1, a.getStringAttribute(), "en");

        final Object res = q.getSingleResult();
        assertNotNull(res);
        assertEquals(a.getUri(), res);
    }

    @Test
    public void selectWithOptionalReturnsNullInUnfilledColumns() {
        final String query =
                "SELECT ?x ?s WHERE { ?x a <" + Vocabulary.C_OWL_CLASS_E + "> ." +
                        " OPTIONAL {?x <" + Vocabulary.P_A_STRING_ATTRIBUTE + "> ?s . } }";
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
    void testCreateNamedNativeQueryWithParameterSetting() {
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final List res = getEntityManager().createNamedQuery("OWLClassA.findByString")
                                           .setParameter("str", a.getStringAttribute(), "en").getResultList();
        assertFalse(res.isEmpty());
        assertTrue(res.contains(a.getUri()));
    }

    @Test
    void queryWithMappingReturnsResultWithVariablesMappedAccordingly() {
        final List res = getEntityManager().createNativeQuery("SELECT * WHERE {" +
                "?x a <" + Vocabulary.C_OWL_CLASS_A + "> ;" +
                "<" + Vocabulary.P_A_STRING_ATTRIBUTE + "> ?y ." +
                "}", OWLClassA.VARIABLE_MAPPING).getResultList();
        final Map<String, Object> expected = new HashMap<>();
        QueryTestEnvironment.getData(OWLClassA.class)
                            .forEach(a -> expected.put(a.getUri().toString(),
                                    new LangString(a.getStringAttribute(), "en")));
        assertEquals(expected.size(), res.size());
        for (Object row : res) {
            assertInstanceOf(Object[].class, row);
            final Object[] elems = (Object[]) row;
            assertEquals(2, elems.length);
            assertTrue(expected.containsKey(elems[0]));
            assertEquals(expected.get(elems[0]), elems[1]);
        }
    }

    @Test
    void queryWithConstructorMappingReturnsCorrectInstances() {
        final List res = getEntityManager().createNativeQuery("SELECT * WHERE {" +
                                                   "?x a <" + Vocabulary.C_OWL_CLASS_A + "> ;" +
                                                   "<" + Vocabulary.P_A_STRING_ATTRIBUTE + "> ?y ." +
                                                   "}", OWLClassA.CONSTRUCTOR_MAPPING)
                                           .getResultList();
        final Map<URI, OWLClassA> expected = new HashMap<>();
        QueryTestEnvironment.getData(OWLClassA.class).forEach(a -> expected.put(a.getUri(), a));

        verifyOWLClassAInstances(res, expected);
    }

    private void verifyOWLClassAInstances(List res, Map<URI, OWLClassA> expected) {
        for (Object item : res) {
            assertInstanceOf(OWLClassA.class, item);
            final OWLClassA a = (OWLClassA) item;
            assertTrue(expected.containsKey(a.getUri()));
            assertEquals(expected.get(a.getUri()).getStringAttribute(), a.getStringAttribute());
        }
    }

    @Test
    void queryWithEntityMappingReturnsCorrectManagedInstances() {
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
    void queryWithEntityMappingLoadsReferencedEntitiesAsWell() {
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
            assertInstanceOf(OWLClassD.class, row);
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
                                           .setParameter("rdfsLabel", URI.create(RDFS.LABEL))
                                           .setParameter("hasDescription", URI.create(DC.Terms.DESCRIPTION))
                                           .setParameter("hasInt", URI.create(Vocabulary.P_T_INTEGER_ATTRIBUTE))
                                           .getResultList();
        final Map<URI, OWLClassT> expected = new HashMap<>();
        QueryTestEnvironment.getData(OWLClassT.class).forEach(t -> expected.put(t.getUri(), t));

        assertEquals(expected.size(), res.size());
        for (Object row : res) {
            assertInstanceOf(OWLClassT.class, row);
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
        final String update = "DELETE { ?inst ?property ?origValue . } WHERE { ?inst ?property ?origValue . }";
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
    void settingStringParameterEscapesTheParameterValue() {
        final EntityManager em = getEntityManager();
        final String query = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "SELECT ?x WHERE { ?x rdfs:comment ?comment . }";
        final String paramValue = "string\nWith\nNewlines";
        final List result = em.createNativeQuery(query).setParameter("comment", paramValue, "en").getResultList();
        assertTrue(result.isEmpty());   // The point here is that no exception is thrown and a result is returned
    }

    @Test
    void querySupportsProcessingResultsUsingStream() {
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final Set<String> types = a.getTypes();
        types.add(a.getClass().getAnnotation(OWLClass.class).iri());
        final String query = "SELECT ?x WHERE { ?instance a ?x . }";
        final Query q = getEntityManager().createNativeQuery(query).setParameter("instance", a.getUri());

        final Set<String> result = (Set<String>) q.getResultStream().map(Object::toString).collect(Collectors.toSet());
        assertTrue(result.containsAll(types));
        assertThat(result.size(), greaterThanOrEqualTo(types.size()));
    }

    @Test
    void selectionByObjectPropertySupportsEntityAsQueryParameter() {
        final OWLClassD d = QueryTestEnvironment.getData(OWLClassD.class).get(0);
        final String query = "SELECT ?x WHERE { ?x a ?type ; ?hasA ?y . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_D))
         .setParameter("hasA", URI.create(Vocabulary.P_HAS_OWL_CLASS_A)).setParameter("y", d.getOwlClassA());

        final List res = q.getResultList();

        assertEquals(1, res.size());
        final Object subRes = res.get(0);
        assertEquals(d.getUri(), subRes);
    }

    /**
     * Enhancement #101
     */
    @Test
    public void selectTypesWithDisableInferenceQueryHintReturnsOnlyAssertedTypes() throws Exception {
        final String superType = Vocabulary.CLASS_IRI_BASE + "A-superclass";
        persistTestData(Collections.singleton(
                        new Quad(URI.create(Vocabulary.C_OWL_CLASS_A), URI.create(RDFS.SUB_CLASS_OF), URI.create(superType))),
                getEntityManager());
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final Set<String> types = new HashSet<>(a.getTypes());
        types.add(a.getClass().getAnnotation(OWLClass.class).iri());
        final String query = "SELECT DISTINCT ?type WHERE { ?instance a ?type . }";
        final Query q = getEntityManager().createNativeQuery(query);
        q.setParameter("instance", a.getUri())
         .setHint(QueryHints.DISABLE_INFERENCE, true);

        final List res = q.getResultList();
        // The result can contain more types (inference)
        assertEquals(types, res.stream().map(Object::toString).collect(Collectors.toSet()));
    }

    @Test
    public void setParameterSupportsLangStringValue() {
        final List<OWLClassA> aInstances = QueryTestEnvironment.getData(OWLClassA.class);
        final List<OWLClassA> matching = aInstances.subList(0, Generators.randomPositiveInt(3, aInstances.size()));
        final Set<LangString> params = matching.stream().map(a -> new LangString(a.getStringAttribute(),
                                                       TestEnvironment.PERSISTENCE_LANGUAGE))
                                               .collect(Collectors.toSet());
        final EntityManager em = getEntityManager();
        final String query = "SELECT ?x WHERE { ?x ?stringAtt ?value . FILTER (?value IN (?params)) }";
        final List result =
                em.createNativeQuery(query).setParameter("stringAtt", URI.create(Vocabulary.P_A_STRING_ATTRIBUTE))
                  .setParameter("params", params)
                  .getResultList();
        assertEquals(matching.size(), result.size());
        matching.forEach(a -> assertThat((List<URI>) result, hasItem(a.getUri())));
    }

    @Test
    protected void querySupportsPluralValuesParameter() {
        final OWLClassA entityA = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        final OWLClassB entityB = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassB.class));
        final List result = getEntityManager().createNativeQuery("SELECT ?x ?type WHERE { ?x a ?type . }")
                                              .setParameter("x", List.of(entityA.getUri(), entityB.getUri()))
                                              .getResultList();
        assertThat((List<URI[]>) result, hasItem(new URI[]{entityA.getUri(), URI.create(Vocabulary.C_OWL_CLASS_A)}));
        entityA.getTypes()
               .forEach(t -> assertThat((List<URI[]>) result, hasItem(new URI[]{entityA.getUri(), URI.create(t)})));
        assertThat((List<URI[]>) result, hasItem(new URI[]{entityB.getUri(), URI.create(Vocabulary.C_OWL_CLASS_B)}));
    }
}
