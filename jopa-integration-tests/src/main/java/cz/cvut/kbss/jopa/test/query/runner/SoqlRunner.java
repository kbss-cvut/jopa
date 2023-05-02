/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.test.query.runner;

import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

import static cz.cvut.kbss.jopa.test.environment.util.ContainsSameEntities.containsSameEntities;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.jupiter.api.Assertions.*;

public abstract class SoqlRunner extends BaseQueryRunner {

    protected SoqlRunner(Logger logger, DataAccessor dataAccessor) {
        super(logger, dataAccessor);
    }

    @Test
    public void testSimpleFindAll() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        final List<OWLClassA> result = getEntityManager().createQuery("SELECT a FROM OWLClassA a", OWLClassA.class)
                                                         .getResultList();
        assertEquals(expected.size(), result.size());
        for (OWLClassA a : result) {
            assertNotNull(a.getStringAttribute());
            assertTrue(expected.stream().anyMatch(aa -> aa.getUri().equals(a.getUri())));
        }
    }

    @Test
    public void testSimpleCount() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        final Object result = getEntityManager().createQuery("SELECT DISTINCT COUNT(a) FROM OWLClassA a", Integer.class)
                                                .getSingleResult();
        assertEquals(expected.size(), result);
    }

    @Test
    public void testFindByDataPropertyAttribute() {
        final OWLClassA expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        final OWLClassA result = getEntityManager()
                .createQuery("SELECT a FROM OWLClassA a WHERE a.stringAttribute = :str", OWLClassA.class)
                .setParameter("str", expected.getStringAttribute(), "en").getSingleResult();
        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getStringAttribute(), result.getStringAttribute());
        assertEquals(expected.getTypes(), result.getTypes());
    }

    @Test
    public void testFindByDataNotPropertyAttribute() {
        final OWLClassA unexpected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        final List<OWLClassA> result = getEntityManager()
                .createQuery("SELECT a FROM OWLClassA a WHERE NOT a.stringAttribute = :str", OWLClassA.class)
                .setParameter("str", unexpected.getStringAttribute(), "en").getResultList();
        for (OWLClassA item : result) {
            assertNotEquals(unexpected.getUri(), item.getUri());
            assertNotEquals(unexpected.getStringAttribute(), item.getStringAttribute());
        }
    }

    @Test
    public void testFindByDataNotPropertyAttributeAndPropertyAttribute() {
        final OWLClassT unexpected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        final int intThreshold = QueryTestEnvironment.getData(OWLClassT.class).size() / 2;
        final List<OWLClassT> result = getEntityManager()
                .createQuery("SELECT t FROM OWLClassT t WHERE NOT t.owlClassA = :a AND t.intAttribute < :intAtt",
                             OWLClassT.class)
                .setParameter("a", unexpected.getOwlClassA().getUri())
                .setParameter("intAtt", intThreshold).getResultList();
        assertFalse(result.isEmpty());
        for (OWLClassT item : result) {
            assertNotEquals(unexpected.getUri(), item.getUri());
            assertTrue(intThreshold > item.getIntAttribute());
        }
    }

    @Test
    public void testFindByObjectPropertyAttribute() {
        final OWLClassD expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassD.class));
        final OWLClassD result = getEntityManager()
                .createQuery("SELECT d FROM OWLClassD d WHERE d.owlClassA = :a", OWLClassD.class)
                .setParameter("a", expected.getOwlClassA().getUri()).getSingleResult();
        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getOwlClassA().getUri(), result.getOwlClassA().getUri());
    }

    @Test
    public void testFindByConjunctionOfAttributes() {
        final OWLClassT sample = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        final List<OWLClassT> result = getEntityManager()
                .createQuery("SELECT t FROM OWLClassT t WHERE t.owlClassA = :a AND t.intAttribute <= :intAtt",
                             OWLClassT.class)
                .setParameter("a", sample.getOwlClassA().getUri())
                .setParameter("intAtt", sample.getIntAttribute()).getResultList();
        assertFalse(result.isEmpty());
        for (OWLClassT item : result) {
            assertEquals(sample.getOwlClassA().getUri(), item.getOwlClassA().getUri());
            assertThat(item.getIntAttribute(), lessThanOrEqualTo(sample.getIntAttribute()));
        }
    }

    @Test
    public void testOrderBy() {
        final List<OWLClassT> expected = QueryTestEnvironment.getData(OWLClassT.class);
        expected.sort(Comparator.comparing(OWLClassT::getIntAttribute));
        final List<OWLClassT> result = getEntityManager()
                .createQuery("SELECT t FROM OWLClassT t ORDER BY t.intAttribute", OWLClassT.class).getResultList();
        assertEquals(expected.size(), result.size());
        for (OWLClassT t : result) {
            assertTrue(expected.stream().anyMatch(tt -> tt.getUri().equals(t.getUri())));
        }
    }

    @Test
    public void testFindByDisjunctionOfAttributes() {
        final OWLClassT sample = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        final List<OWLClassT> result = getEntityManager()
                .createQuery("SELECT t FROM OWLClassT t WHERE t.owlClassA = :a OR t.intAttribute <= :intAtt",
                             OWLClassT.class)
                .setParameter("a", sample.getOwlClassA().getUri())
                .setParameter("intAtt", sample.getIntAttribute()).getResultList();
        assertFalse(result.isEmpty());
        for (OWLClassT item : result) {
            boolean matches = item.getOwlClassA().getUri().equals(sample.getOwlClassA().getUri());
            matches |= item.getIntAttribute() <= sample.getIntAttribute();
            assertTrue(matches);
        }
    }

    @Test
    public void testFindByTransitiveAttributeValue() {
        final OWLClassD expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassD.class));
        final OWLClassD result = getEntityManager()
                .createQuery("SELECT d FROM OWLClassD d WHERE d.owlClassA.stringAttribute = :stringAtt",
                             OWLClassD.class)
                .setParameter("stringAtt", expected.getOwlClassA().getStringAttribute(), "en").getSingleResult();
        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getOwlClassA().getUri(), result.getOwlClassA().getUri());
    }

    @Test
    public void testFindByAttributeInValues() {
        final List<OWLClassA> aInstances = QueryTestEnvironment.getData(OWLClassA.class);
        final List<OWLClassA> matching;
        if (Generators.randomBoolean()) {
            matching = aInstances.subList(0, aInstances.size() / 2);
        } else {
            matching = aInstances.subList(aInstances.size() / 2, aInstances.size());
        }
        final List<OWLClassD> expected = QueryTestEnvironment.getData(OWLClassD.class).stream()
                                                             .filter(d -> matching.stream().anyMatch(
                                                                     a -> d.getOwlClassA().getUri().equals(a.getUri())))
                                                             .collect(Collectors.toList());
        final List<OWLClassD> result =
                getEntityManager().createQuery("SELECT d FROM OWLClassD d WHERE d.owlClassA IN :aInstances",
                                               OWLClassD.class)
                                  .setParameter("aInstances", matching)
                                  .getResultList();
        assertThat(result, containsSameEntities(expected));
    }

    /**
     * Bug #135
     */
    @Test
    public void testJoinOnPluralAttribute() {
        final List<OWLClassJ> jInstances = QueryTestEnvironment.getData(OWLClassJ.class);
        final OWLClassJ matching = Generators.getRandomItem(jInstances);
        final Set<LangString> stringSet =
                matching.getOwlClassA().stream()
                        .map(a -> new LangString(a.getStringAttribute(), TestEnvironment.PERSISTENCE_LANGUAGE))
                        .collect(Collectors.toSet());

        final List<OWLClassJ> result = getEntityManager().createQuery(
                                                                 "SELECT j FROM OWLClassJ j WHERE j.owlClassA.stringAttribute IN :stringSet", OWLClassJ.class)
                                                         .setParameter("stringSet", stringSet).getResultList();
        assertFalse(result.isEmpty());
        assertTrue(result.stream().anyMatch(j -> j.getUri().equals(matching.getUri())));
    }

    /**
     * Enhancement #138
     */
    @Test
    public void testSelectByIdentifier() {
        final OWLClassA instance = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        final OWLClassA result =
                getEntityManager().createQuery("SELECT a FROM OWLClassA a WHERE a.uri = :uri", OWLClassA.class)
                                  .setParameter("uri", instance.getUri())
                                  .getSingleResult();
        assertNotNull(result);
        assertEquals(instance.getUri(), result.getUri());
    }

    @Test
    public void testSelectByLikeWithUppercase() {
        final OWLClassA instance = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        final List<OWLClassA> result =
                getEntityManager().createQuery("SELECT a FROM OWLClassA a WHERE UPPER(a.stringAttribute) LIKE :value",
                                               OWLClassA.class)
                                  .setParameter("value", instance.getStringAttribute().substring(0, 3)
                                                                 .toUpperCase(Locale.ROOT) + ".+")
                                  .getResultList();
        assertFalse(result.isEmpty());
        assertThat(result, hasItem(instance));
    }

    @Test
    public void testSelectByAbsoluteValueOfAnInteger() {
        final List<OWLClassM> instances = QueryTestEnvironment.getData(OWLClassM.class);
        final int value = Math.abs(Generators.randomInt());
        final List<OWLClassM> matching = instances.stream().filter(m -> Math.abs(m.getIntAttribute()) <= value).collect(
                Collectors.toList());
        final List<OWLClassM> result = getEntityManager().createQuery("SELECT m FROM OWLClassM m WHERE ABS(m.intAttribute) <= :value", OWLClassM.class)
                .setParameter("value", value)
                .getResultList();
        assertEquals(matching.size(), result.size());
        matching.forEach(m -> assertTrue(result.stream().anyMatch(rm -> rm.getKey().equals(m.getKey()))));
    }
}
