/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public abstract class TypedQueryRunner extends BaseQueryRunner {

    private static final String SELECT_BY_TYPE = "SELECT ?x WHERE { ?x a ?type .}";

    protected TypedQueryRunner(Logger logger, DataAccessor dataAccessor) {
        super(logger, dataAccessor);
    }

    @Test
    void testFindAll() {
        final TypedQuery<OWLClassD> q =
                getEntityManager().createNativeQuery(SELECT_BY_TYPE, OWLClassD.class).setParameter("type", URI.create(
                        Vocabulary.C_OWL_CLASS_D));
        final List<OWLClassD> ds = new ArrayList<>(QueryTestEnvironment.getDataByContext(null, OWLClassD.class));
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
    void testSelectByTypeAndDataPropertyValue() {
        final OWLClassB b = QueryTestEnvironment.getData(OWLClassB.class).get(5);
        final String query =
                "SELECT ?x WHERE { " +
                        "?x a ?type ; " +
                        "?stringAtt ?bString . }";
        final TypedQuery<OWLClassB> q = getEntityManager().createNativeQuery(query, OWLClassB.class);
        q.setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_B))
                .setParameter("stringAtt", URI.create(Vocabulary.P_B_STRING_ATTRIBUTE))
                .setParameter("bString", b.getStringAttribute(), "en");
        final OWLClassB res = q.getSingleResult();
        assertNotNull(res);
        assertEquals(b.getUri(), res.getUri());
        assertEquals(b.getStringAttribute(), res.getStringAttribute());
    }

    @Test
    void testSelectByObjectProperty() {
        final String query = "SELECT ?x WHERE { ?x a ?type ; ?hasA ?y . }";
        final List<OWLClassD> ds = new ArrayList<>(QueryTestEnvironment.getData(OWLClassD.class));
        final OWLClassA a = ds.get(Generators.randomPositiveInt(2, ds.size())).getOwlClassA();
        final TypedQuery<OWLClassD> q = getEntityManager().createNativeQuery(query, OWLClassD.class)
                .setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_D))
                .setParameter("hasA",
                        URI.create(Vocabulary.P_HAS_OWL_CLASS_A))
                .setParameter("y", a.getUri());

        final List<OWLClassD> expected = ds.stream().filter(d -> d.getOwlClassA().getUri().equals(a.getUri())).collect(
                Collectors.toList());
        final List<OWLClassD> res = q.getResultList();
        assertEquals(expected.size(), res.size());
    }

    @Test
    void testSetMaxResults() {
        final TypedQuery<OWLClassE> q = getEntityManager().createNativeQuery(SELECT_BY_TYPE, OWLClassE.class)
                .setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_E));
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

    @Test
    void testSetMaxResultsNegative() {
        final TypedQuery<OWLClassE> q = getEntityManager().createNativeQuery(SELECT_BY_TYPE, OWLClassE.class)
                .setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_E));
        assertThrows(IllegalArgumentException.class, () -> q.setMaxResults(-1));
    }

    @Test
    void testSetMaxResultsZero() {
        final TypedQuery<OWLClassE> q = getEntityManager().createNativeQuery(SELECT_BY_TYPE, OWLClassE.class)
                .setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_E));
        q.setMaxResults(0);
        final List<OWLClassE> res = q.getResultList();
        assertNotNull(res);
        assertTrue(res.isEmpty());
    }

    @Test
    void testGetSingleResult() {
        final OWLClassA a = QueryTestEnvironment.getData(OWLClassA.class).get(0);
        final String query =
                "SELECT ?x WHERE { ?x ?stringAtt ?aString .}";
        final TypedQuery<OWLClassA> q = getEntityManager().createNativeQuery(query, OWLClassA.class);
        q.setParameter("stringAtt", URI.create(Vocabulary.P_A_STRING_ATTRIBUTE))
                .setParameter("aString", a.getStringAttribute(), "en");
        final OWLClassA res = q.getSingleResult();
        assertNotNull(res);
        assertEquals(a.getUri(), res.getUri());
    }

    @Test
    void testGetSingleResultMultiples() {
        final TypedQuery<OWLClassE> q = getEntityManager().createNativeQuery(SELECT_BY_TYPE, OWLClassE.class)
                .setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_E));
        assertThrows(NoUniqueResultException.class, q::getSingleResult);
    }

    @Test
    void testGetSingleResultNoResult() {
        final String query =
                "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX> . }";
        final TypedQuery<OWLClassE> q = getEntityManager().createNativeQuery(query, OWLClassE.class);
        assertThrows(NoResultException.class, q::getSingleResult);
    }

    @Test
    void testCreateQueryNullQuery() {
        assertThrows(NullPointerException.class, () -> getEntityManager().createNativeQuery(null, OWLClassA.class));
    }

    @Test
    void testCreateQueryNullClass() {
        final String query = "SELECT ?x WHERE { ?x ?y ?z .}";
        assertThrows(NullPointerException.class,
                () -> getEntityManager().createNativeQuery(query, (Class<OWLClassA>) null));
    }

    @Test
    void askQueryReturnsTrue() {
        final String query = "ASK { ?x a ?type . }";
        final TypedQuery<Boolean> q = getEntityManager().createNativeQuery(query, Boolean.class)
                .setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_A));
        final Boolean res = q.getSingleResult();
        assertNotNull(res);
        assertTrue(res);
    }

    @Test
    void askQueryReturnsFalse() {
        final String query = "ASK { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassX> . }";
        final TypedQuery<Boolean> q = getEntityManager().createNativeQuery(query, Boolean.class);
        final List<Boolean> res = q.getResultList();
        assertNotNull(res);
        assertEquals(1, res.size());
        assertFalse(res.get(0));
    }

    @Test
    void askQueryWithPositionParameter() {
        final String query = "ASK { ?x a $1 . }";
        final URI paramValue = URI.create(OWLClassA.class.getAnnotation(OWLClass.class).iri());
        final TypedQuery<Boolean> q = getEntityManager().createNativeQuery(query, Boolean.class)
                .setParameter(1, paramValue);
        final Boolean res = q.getSingleResult();
        assertNotNull(res);
        assertTrue(res);
    }

    @Test
    void testCreateTypedNamedNativeQuery() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        final List<URI> uris = expected.stream().map(OWLClassA::getUri).collect(Collectors.toList());
        final List<OWLClassA> res = getEntityManager().createNamedQuery("OWLClassA.findAll", OWLClassA.class)
                .getResultList();
        assertEquals(expected.size(), res.size());
        res.forEach(a -> assertTrue(uris.contains(a.getUri())));
    }

    @Test
    void usingDescriptorAllowsToCustomizeQueryResults() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        expected.forEach(a -> assertNotNull(a.getStringAttribute()));
        final Descriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage("cs");
        final List<OWLClassA> result = getEntityManager().createNamedQuery("OWLClassA.findAll", OWLClassA.class)
                .setDescriptor(descriptor).getResultList();
        assertEquals(expected.size(), result.size());
        result.forEach(a -> assertNull(a.getStringAttribute()));    // Because the data has @en language tag
    }

    @Test
    public void usingUntypedQueryAllowsToSpecifyLimitInQuery() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        final int size = expected.size() / 2;
        final List<OWLClassA> result = getEntityManager().createNativeQuery("SELECT ?x WHERE {" +
                        "?x a ?classA . } LIMIT ?limit", OWLClassA.class)
                .setParameter("classA", URI.create(Vocabulary.C_OWL_CLASS_A))
                .setUntypedParameter("limit", size).getResultList();
        assertEquals(size, result.size());
    }

    @Test
    public void setFirstResultCanBeUsedToOffsetFirstQueryResult() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        expected.sort(Comparator.comparing(OWLClassA::getUri));
        final int offset = expected.size() / 2;
        final List<OWLClassA> result = getEntityManager().createNamedQuery("OWLClassA.findAll", OWLClassA.class)
                .setFirstResult(offset).getResultList();
        assertEquals(expected.size() - offset, result.size());
        for (int i = 0; i < result.size(); i++) {
            assertEquals(expected.get(i + offset).getUri(), result.get(i).getUri());
        }
    }

    @Test
    void querySupportsProcessingResultsUsingStream() {
        final TypedQuery<OWLClassD> q =
                getEntityManager().createNativeQuery(SELECT_BY_TYPE, OWLClassD.class).setParameter("type", URI.create(
                        Vocabulary.C_OWL_CLASS_D));
        final List<OWLClassD> dList = QueryTestEnvironment.getData(OWLClassD.class);
        final Set<URI> expected = dList.stream().map(OWLClassD::getUri).collect(Collectors.toSet());

        q.getResultStream().map(OWLClassD::getUri).forEach(rUri -> assertTrue(expected.contains(rUri)));
        assertEquals(dList.size(), (int) q.getResultStream().count());
    }

    @Test
    void selectionByObjectPropertySupportsEntityAsQueryParameter() {
        final String query = "SELECT ?x WHERE { ?x a ?type ; ?hasA ?y . }";
        final List<OWLClassD> ds = new ArrayList<>(QueryTestEnvironment.getData(OWLClassD.class));
        final OWLClassA a = ds.get(Generators.randomPositiveInt(2, ds.size())).getOwlClassA();
        final TypedQuery<OWLClassD> q = getEntityManager().createNativeQuery(query, OWLClassD.class)
                .setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_D))
                .setParameter("hasA",
                        URI.create(Vocabulary.P_HAS_OWL_CLASS_A))
                .setParameter("y", a);

        final List<OWLClassD> expected = ds.stream().filter(d -> d.getOwlClassA().getUri().equals(a.getUri()))
                .sorted(Comparator.comparing(OWLClassD::getUri))
                .collect(Collectors.toList());
        final List<OWLClassD> res = q.getResultList();
        res.sort(Comparator.comparing(OWLClassD::getUri));
        assertEquals(expected.size(), res.size());
        for (int i = 0; i < expected.size(); i++) {
            assertEquals(expected.get(i).getUri(), res.get(i).getUri());
            assertNotNull(res.get(i).getOwlClassA());
        }
    }

    @Test
    protected void querySupportsCollectionParameters() {
        final String query = "SELECT ?x WHERE { ?x a ?type . FILTER (?x IN (?values)) }";
        final List<OWLClassA> as = QueryTestEnvironment.getData(OWLClassA.class).stream()
                .filter(a -> Generators.randomBoolean())
                .collect(Collectors.toList());
        final TypedQuery<OWLClassA> q = getEntityManager().createNativeQuery(query, OWLClassA.class)
                .setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_A))
                .setParameter("values", as.stream().map(OWLClassA::getUri)
                        .collect(Collectors.toList()));
        final List<OWLClassA> result = q.getResultList();
        assertEquals(as.size(), result.size());
        for (OWLClassA exp : as) {
            assertTrue(result.stream().anyMatch(a -> a.getUri().equals(exp.getUri())));
        }
    }

    @Test
    protected void querySupportsSelectionByDate() {
        final long now = System.currentTimeMillis() / 1000L;
        final List<OWLClassM> mInstances = IntStream.range(0, 10).mapToObj(i -> {
            final OWLClassM m = new OWLClassM();
            m.setKey(Generators.generateUri().toString());
            // Now minus i * hour
            m.setDateAttribute(new Date(now - i * 60 * 60L));
            return m;
        }).collect(Collectors.toList());
        getEntityManager().getTransaction().begin();
        mInstances.forEach(getEntityManager()::persist);
        getEntityManager().getTransaction().commit();
        final LocalDateTime param = LocalDateTime.now().truncatedTo(ChronoUnit.SECONDS).minusHours(3);
        final List<OWLClassM> matching = mInstances.stream()
                .filter(m -> m.getDateAttribute().toInstant().atOffset(ZoneOffset.UTC).isBefore(param.atOffset(ZoneOffset.UTC)))
                .collect(Collectors.toList());
        try {
            final List<OWLClassM> result = getEntityManager().createQuery("SELECT m FROM OWLClassM m WHERE m.dateAttribute < :date", OWLClassM.class)
                    .setParameter("date", param).getResultList();
            assertEquals(matching.size(), result.size());
            matching.forEach(m -> assertTrue(result.stream().anyMatch(rm -> rm.getKey().equals(m.getKey()))));
        } finally {
            cleanupTestData(Vocabulary.C_OWL_CLASS_M);
        }
    }

    private void cleanupTestData(String type) {
        getEntityManager().getTransaction().begin();
        getEntityManager().createNativeQuery("DELETE WHERE { ?x a ?type . ?x ?y ?z . }")
                .setParameter("type", URI.create(type)).executeUpdate();
        getEntityManager().getTransaction().commit();
    }

    @Test
    protected void querySupportsSelectionByEntityIdentifier() {
        final OWLClassA entity = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        final OWLClassA result = getEntityManager().createNativeQuery("SELECT ?x WHERE { ?x a ?type . }", OWLClassA.class)
                .setParameter("type", URI.create(Vocabulary.C_OWL_CLASS_A))
                .setParameter("x", entity.getUri())
                .getSingleResult();
        assertEquals(entity.getUri(), result.getUri());
    }
}
