/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.model.query.criteria.Root;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassA_;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassJ;
import cz.cvut.kbss.jopa.test.OWLClassM;
import cz.cvut.kbss.jopa.test.OWLClassT;
import cz.cvut.kbss.jopa.test.OWLClassY;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.net.URI;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static cz.cvut.kbss.jopa.test.environment.util.ContainsSameEntities.containsSameEntities;
import static java.util.function.Predicate.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public abstract class CriteriaRunner extends BaseQueryRunner {

    protected CriteriaRunner(Logger logger, DataAccessor dataAccessor) {
        super(logger, dataAccessor);
    }

    @Test
    public void testSimpleFindAll() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root);
        TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query);
        final List<OWLClassA> result = tq.getResultList();

        assertEquals(expected.size(), result.size());
        for (OWLClassA a : result) {
            assertNotNull(a.getStringAttribute());
            assertTrue(expected.stream().anyMatch(aa -> aa.getUri().equals(a.getUri())));
        }
    }

    @Test
    public void testSimpleCount() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<Integer> query = cb.createQuery(Integer.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(cb.count(root));
        final Integer result = getEntityManager().createQuery(query).getSingleResult();

        assertEquals(expected.size(), result);
    }

    @Test
    public void testFindByDataPropertyAttribute() {
        final OWLClassA expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        Predicate restriction = cb.equal(root.getAttr(OWLClassA_.stringAttribute), expected.getStringAttribute(), "en");
        query.select(root).where(restriction);
        TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query);
        final OWLClassA result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getStringAttribute(), result.getStringAttribute());
        assertEquals(expected.getTypes(), result.getTypes());
    }

    @Test
    public void testFindByDataNotPropertyAttribute() {
        final OWLClassA unexpected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        Predicate restriction =
                cb.equal(root.getAttr(OWLClassA_.stringAttribute), unexpected.getStringAttribute(), "en");
        query.select(root).where(cb.not(restriction));
        TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query);
        final List<OWLClassA> result = tq.getResultList();

        for (OWLClassA item : result) {
            assertNotEquals(unexpected.getUri(), item.getUri());
            assertNotEquals(unexpected.getStringAttribute(), item.getStringAttribute());
        }
    }

    @Test
    public void testFindByDataNotPropertyAttributeAndPropertyAttribute() {
        final OWLClassT unexpected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        final int intThreshold = QueryTestEnvironment.getData(OWLClassT.class).size() / 2;
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassT> query = cb.createQuery(OWLClassT.class);
        Root<OWLClassT> root = query.from(OWLClassT.class);
        Predicate firstRestriction = cb.not(cb.equal(root.getAttr("owlClassA"), unexpected.getOwlClassA().getUri()));
        Predicate secondRestriction = cb.lessThan(root.getAttr("intAttribute"), intThreshold);
        Predicate restrictions = cb.and(firstRestriction, secondRestriction);
        query.select(root).where(restrictions);
        TypedQuery<OWLClassT> tq = getEntityManager().createQuery(query);
        final List<OWLClassT> result = tq.getResultList();

        assertFalse(result.isEmpty());
        for (OWLClassT item : result) {
            assertNotEquals(unexpected.getUri(), item.getUri());
            assertTrue(intThreshold > item.getIntAttribute());
        }
    }

    @Test
    public void testFindByObjectPropertyAttribute() {
        final OWLClassD expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassD.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassD> query = cb.createQuery(OWLClassD.class);
        Root<OWLClassD> root = query.from(OWLClassD.class);
        Predicate restriction = cb.equal(root.getAttr("owlClassA"), expected.getOwlClassA().getUri());
        query.select(root).where(restriction);
        TypedQuery<OWLClassD> tq = getEntityManager().createQuery(query);
        final OWLClassD result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getOwlClassA().getUri(), result.getOwlClassA().getUri());
    }

    @Test
    public void testFindByConjunctionOfAttributes() {
        final OWLClassT sample = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassT> query = cb.createQuery(OWLClassT.class);
        Root<OWLClassT> root = query.from(OWLClassT.class);
        Predicate firstRestriction = cb.equal(root.getAttr("owlClassA"), sample.getOwlClassA().getUri());
        Predicate secondRestriction = cb.lessThanOrEqual(root.getAttr("intAttribute"), sample.getIntAttribute());
        query.select(root).where(firstRestriction, secondRestriction);
        TypedQuery<OWLClassT> tq = getEntityManager().createQuery(query);
        final List<OWLClassT> result = tq.getResultList();

        assertFalse(result.isEmpty());
        for (OWLClassT item : result) {
            assertEquals(sample.getOwlClassA().getUri(), item.getOwlClassA().getUri());
            assertThat(item.getIntAttribute(), lessThanOrEqualTo(sample.getIntAttribute()));
        }
    }

    @Test
    public void testFindByConjunctionOfAttributesInList() {
        final OWLClassT sample = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassT> query = cb.createQuery(OWLClassT.class);
        Root<OWLClassT> root = query.from(OWLClassT.class);
        List<Predicate> restrictions = new ArrayList<>();
        restrictions.add(cb.equal(root.getAttr("owlClassA"), sample.getOwlClassA().getUri()));
        restrictions.add(cb.lessThanOrEqual(root.getAttr("intAttribute"), sample.getIntAttribute()));
        query.select(root).where(restrictions);
        TypedQuery<OWLClassT> tq = getEntityManager().createQuery(query);
        final List<OWLClassT> result = tq.getResultList();

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
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassT> query = cb.createQuery(OWLClassT.class);
        Root<OWLClassT> root = query.from(OWLClassT.class);
        query.select(root).orderBy(cb.asc(root.getAttr("intAttribute")));
        TypedQuery<OWLClassT> tq = getEntityManager().createQuery(query);
        final List<OWLClassT> result = tq.getResultList();

        assertEquals(expected.size(), result.size());
        for (OWLClassT t : result) {
            assertTrue(expected.stream().anyMatch(tt -> tt.getUri().equals(t.getUri())));
        }
    }

    @Test
    public void testFindByDisjunctionOfAttributes() {
        final OWLClassT sample = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassT.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassT> query = cb.createQuery(OWLClassT.class);
        Root<OWLClassT> root = query.from(OWLClassT.class);
        Predicate firstRestriction = cb.equal(root.getAttr("owlClassA"), sample.getOwlClassA().getUri());
        Predicate secondRestriction = cb.lessThanOrEqual(root.getAttr("intAttribute"), sample.getIntAttribute());
        query.select(root).where(cb.or(firstRestriction, secondRestriction));
        TypedQuery<OWLClassT> tq = getEntityManager().createQuery(query);
        final List<OWLClassT> result = tq.getResultList();

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
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassD> query = cb.createQuery(OWLClassD.class);
        Root<OWLClassD> root = query.from(OWLClassD.class);
        Predicate restrictions = cb.equal(root.getAttr("owlClassA").getAttr("stringAttribute"),
                expected.getOwlClassA().getStringAttribute(), "en");
        query.select(root).where(restrictions);
        TypedQuery<OWLClassD> tq = getEntityManager().createQuery(query);
        final OWLClassD result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getOwlClassA().getUri(), result.getOwlClassA().getUri());
    }

    @Test
    public void testFindByParameterExpression() {
        final OWLClassA expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        final ParameterExpression<String> strAtt = cb.parameter(String.class, "pOne");
        Predicate restriction = cb.equal(root.getAttr(OWLClassA_.stringAttribute), strAtt);
        query.select(root).where(restriction);
        TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query);
        tq.setParameter(strAtt, expected.getStringAttribute(), "en");
        final OWLClassA result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getStringAttribute(), result.getStringAttribute());
        assertEquals(expected.getTypes(), result.getTypes());
    }

    @Test
    public void testFindByUnnamedParameterExpression() {
        final OWLClassA expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        final ParameterExpression<String> strAtt = cb.parameter(String.class);
        Predicate restriction = cb.equal(root.getAttr(OWLClassA_.stringAttribute), strAtt);
        query.select(root).where(restriction);
        TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query);
        tq.setParameter(strAtt, expected.getStringAttribute(), "en");
        final OWLClassA result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getStringAttribute(), result.getStringAttribute());
        assertEquals(expected.getTypes(), result.getTypes());
    }

    @Test
    public void testFindByLiteral() {
        final OWLClassA expected = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        Predicate restriction =
                cb.equal(root.getAttr(OWLClassA_.stringAttribute), cb.literal(expected.getStringAttribute(), "en"));
        query.select(root).where(restriction);
        TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query);
        final OWLClassA result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getStringAttribute(), result.getStringAttribute());
        assertEquals(expected.getTypes(), result.getTypes());
    }

    @Test
    public void testFindByAttributeValueIn() {
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
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        CriteriaQuery<OWLClassD> query = cb.createQuery(OWLClassD.class);
        Root<OWLClassD> root = query.from(OWLClassD.class);
        query.select(root).where(root.getAttr("owlClassA").in(matching));

        final List<OWLClassD> result = getEntityManager().createQuery(query).getResultList();
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

        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassJ> query = cb.createQuery(OWLClassJ.class);
        final Root<OWLClassJ> root = query.from(OWLClassJ.class);
        query.select(root).where(root.getAttr("owlClassA").getAttr("stringAttribute").in(stringSet));

        final List<OWLClassJ> result = getEntityManager().createQuery(query).getResultList();
        assertFalse(result.isEmpty());
        assertTrue(result.stream().anyMatch(j -> j.getUri().equals(matching.getUri())));
    }

    @Test
    public void testQueryOnlyRootWithEmptyWhereClauseWorks() {
        final List<OWLClassA> aInstances = QueryTestEnvironment.getData(OWLClassA.class);
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        final Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).where();

        final List<OWLClassA> result = getEntityManager().createQuery(query).getResultList();
        assertThat(result, containsSameEntities(aInstances));
    }

    /**
     * Enhancement #138
     */
    @Test
    public void testSelectByIdentifierEquality() {
        final OWLClassA instance = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        final Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).where(cb.equal(root.getAttr("uri"), instance.getUri()));

        final OWLClassA result = getEntityManager().createQuery(query).getSingleResult();
        assertEquals(instance.getUri(), result.getUri());
    }

    /**
     * Enhancement #138
     */
    @Test
    public void testSelectByIdentifierInCollection() {
        final List<OWLClassA> matchingInstances = QueryTestEnvironment.getData(OWLClassA.class).subList(0,
                Generators.randomPositiveInt(2, QueryTestEnvironment.getData(OWLClassA.class).size()));
        final List<URI> ids = matchingInstances.stream().map(OWLClassA::getUri).collect(Collectors.toList());
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        final Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).where(root.getAttr("uri").in(ids));

        final List<OWLClassA> result = getEntityManager().createQuery(query).getResultList();
        assertThat(result, containsSameEntities(matchingInstances));
    }

    @Test
    public void testSelectUsingStringUppercaseFunction() {
        final List<OWLClassA> instances = QueryTestEnvironment.getData(OWLClassA.class);
        final OWLClassA sample = Generators.getRandomItem(instances);
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        final Root<OWLClassA> root = query.from(OWLClassA.class);
        final ParameterExpression<String> param = cb.parameter(String.class);
        query.select(root).where(cb.like(cb.upper(root.getAttr("stringAttribute")), param));

        final List<OWLClassA> result = getEntityManager().createQuery(query)
                                                         .setParameter(param,
                                                                 sample.getStringAttribute().substring(0, 5)
                                                                       .toUpperCase(Locale.ROOT) + ".+")
                                                         .getResultList();
        assertFalse(result.isEmpty());
        assertThat(result, hasItem(sample));
    }

    @Test
    public void testSelectUsingMathFloorFunction() {
        final List<OWLClassM> instances = QueryTestEnvironment.getData(OWLClassM.class);
        final OWLClassM match = Generators.getRandomItem(instances);
        final double value = Math.floor(match.getDoubleAttribute());
        final CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
        final Root<OWLClassM> root = query.from(OWLClassM.class);
        final ParameterExpression<Double> param = cb.parameter(Double.class);
        query.select(root).where(cb.equal(cb.floor(root.getAttr("doubleAttribute")), param));

        final List<OWLClassM> result = getEntityManager().createQuery(query)
                                                         .setParameter(param, value)
                                                         .getResultList();
        assertFalse(result.isEmpty());
        assertTrue(result.stream().anyMatch(rm -> rm.getKey().equals(match.getKey())));
    }

    @Test
    public void testSelectByIdentifierAndAttributeEquality() {
        final OWLClassA instance = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        final Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).where(
                cb.equal(root.getAttr("stringAttribute"), instance.getStringAttribute(), TestEnvironment.PERSISTENCE_LANGUAGE),
                cb.equal(root.getAttr("uri"), instance.getUri())
        );

        final OWLClassA result = getEntityManager().createQuery(query).getSingleResult();
        assertEquals(instance.getUri(), result.getUri());
    }

    /**
     * Bug #178.
     */
    @Test
    public void selectByIdAndRelatedAttributeValueIsCommutative() {
        final OWLClassD instance = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassD.class));
        CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassD> queryIdFirst = cb.createQuery(OWLClassD.class);
        final Root<OWLClassD> rootIdFirst = queryIdFirst.from(OWLClassD.class);
        queryIdFirst.select(rootIdFirst).where(
                cb.equal(rootIdFirst.getAttr("uri"), instance.getUri()),
                cb.equal(rootIdFirst.getAttr("owlClassA").getAttr("stringAttribute"), instance.getOwlClassA()
                                                                                              .getStringAttribute(), TestEnvironment.PERSISTENCE_LANGUAGE));

        final OWLClassD resultIfFirst = getEntityManager().createQuery(queryIdFirst).getSingleResult();
        assertEquals(instance.getUri(), resultIfFirst.getUri());


        final CriteriaQuery<OWLClassD> queryIdSecond = cb.createQuery(OWLClassD.class);
        final Root<OWLClassD> rootIdSecond = queryIdSecond.from(OWLClassD.class);
        queryIdSecond.select(rootIdSecond).where(
                cb.equal(rootIdSecond.getAttr("owlClassA").getAttr("stringAttribute"), instance.getOwlClassA()
                                                                                               .getStringAttribute(), TestEnvironment.PERSISTENCE_LANGUAGE),
                cb.equal(rootIdSecond.getAttr("uri"), instance.getUri()));
        final OWLClassD resultIdSecond = getEntityManager().createQuery(queryIdSecond).getSingleResult();
        assertEquals(instance.getUri(), resultIdSecond.getUri());
    }

    @Test
    public void selectByLanguageTag() {
        final String language = Generators.getRandomItem(List.of("de", "cs", "fr"));
        final List<OWLClassY> expected = QueryTestEnvironment.getData(OWLClassY.class).stream()
                                                             .filter(y -> y.getSingularString().getLanguages()
                                                                           .contains(language))
                                                             .collect(Collectors.toList());
        final CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassY> query = cb.createQuery(OWLClassY.class);
        final Root<OWLClassY> root = query.from(OWLClassY.class);
        query.select(root).where(cb.equal(cb.lang(root.getAttr("singularString")), language));

        final List<OWLClassY> result = getEntityManager().createQuery(query).getResultList();
        assertThat(result, containsSameEntities(expected));
    }

    @Test
    public void selectByTypeContainingMember() {
        final OWLClassA sample = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        final Optional<String> type = sample.getTypes().stream().filter(not(QueryTestEnvironment.COMMON_TYPE::equals))
                                            .findFirst();
        assert type.isPresent();
        final CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        final Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).where(cb.isMember(URI.create(type.get()), root.getAttr("types")));

        final List<OWLClassA> result = getEntityManager().createQuery(query).getResultList();
        assertFalse(result.isEmpty());
        result.forEach(r -> assertThat(r.getTypes(), hasItem(type.get())));
    }

    @Test
    void selectByMemberOfRdfContainer() {
        runSelectByMemberOf("rdfBag");
    }

    private void runSelectByMemberOf(String attName) {
        final List<OWLClassC> allCs = QueryTestEnvironment.generateOwlClassCInstances(getEntityManager());
        final OWLClassA sample = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        // All attributes contain the same list of OWLClassA references
        final List<OWLClassC> owners = allCs.stream().filter(c -> c.getRdfBag().stream()
                                                                   .anyMatch(a -> a.getUri().equals(sample.getUri())))
                                            .toList();
        final CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassC> query = cb.createQuery(OWLClassC.class);
        final Root<OWLClassC> root = query.from(OWLClassC.class);
        query.select(root).where(cb.isMember(sample, root.getAttr(attName)));

        final TypedQuery<OWLClassC> tq = getEntityManager().createQuery(query);
        final List<OWLClassC> result = tq.getResultList();
        assertEquals(owners.size(), result.size());
        assertThat(result, containsSameEntities(owners));
    }

    @Test
    void selectByMemberOfRdfCollection() {
        runSelectByMemberOf("rdfCollection");
    }

    @Test
    void selectByMemberOfSimpleList() {
        runSelectByMemberOf("simpleList");
    }

    @Test
    void selectByMemberOfReferencedList() {
        runSelectByMemberOf("referencedList");
    }

    @Test
    void selectByMemberOfRdfContainerAndSimpleList() {
        final List<OWLClassC> allCs = QueryTestEnvironment.generateOwlClassCInstances(getEntityManager());
        final OWLClassA sample = Generators.getRandomItem(QueryTestEnvironment.getData(OWLClassA.class));
        // All attributes contain the same list of OWLClassA references
        final List<OWLClassC> owners = allCs.stream().filter(c -> c.getRdfBag().stream()
                                                                   .anyMatch(a -> a.getUri().equals(sample.getUri())))
                                            .toList();
        final CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassC> query = cb.createQuery(OWLClassC.class);
        final Root<OWLClassC> root = query.from(OWLClassC.class);
        query.select(root)
             .where(cb.isMember(sample, root.getAttr("rdfBag")), cb.isMember(sample, root.getAttr("simpleList")));

        final TypedQuery<OWLClassC> tq = getEntityManager().createQuery(query);
        final List<OWLClassC> result = tq.getResultList();
        assertEquals(owners.size(), result.size());
        assertThat(result, containsSameEntities(owners));
    }

    @Test
    void testSelectionByLangMatches() {
        final List<OWLClassA> expected = QueryTestEnvironment.getData(OWLClassA.class);
        final CriteriaBuilder cb = getEntityManager().getCriteriaBuilder();
        final CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        final Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root)
             .where(cb.langMatches(cb.lang(root.getAttr("stringAttribute")), TestEnvironment.PERSISTENCE_LANGUAGE));

        final TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query);
        final List<OWLClassA> result = tq.getResultList();
        assertEquals(expected.size(), result.size());
        assertThat(result, containsSameEntities(expected));
    }
}
