/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.model.query.criteria.*;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;
import cz.cvut.kbss.jopa.test.*;
import cz.cvut.kbss.jopa.test.environment.DataAccessor;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.jupiter.api.Assertions.*;

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
        Predicate restriction = cb.equal(root.getAttr(OWLClassA_.stringAttribute),expected.getStringAttribute(),"en");
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
        Predicate restriction = cb.equal(root.getAttr(OWLClassA_.stringAttribute), unexpected.getStringAttribute(),"en");
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
        Predicate restrictions = cb.and(firstRestriction,secondRestriction);
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
        Predicate restriction = cb.equal(root.getAttr("owlClassA"),expected.getOwlClassA().getUri());
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
        Predicate firstRestriction = cb.equal(root.getAttr("owlClassA"),sample.getOwlClassA().getUri());
        Predicate secondRestriction = cb.lessThanOrEqual(root.getAttr("intAttribute"),sample.getIntAttribute());
        query.select(root).where(firstRestriction,secondRestriction);
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
        restrictions.add(cb.equal(root.getAttr("owlClassA"),sample.getOwlClassA().getUri()));
        restrictions.add(cb.lessThanOrEqual(root.getAttr("intAttribute"),sample.getIntAttribute()));
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
        Predicate firstRestriction = cb.equal(root.getAttr("owlClassA"),sample.getOwlClassA().getUri());
        Predicate secondRestriction = cb.lessThanOrEqual(root.getAttr("intAttribute"),sample.getIntAttribute());
        query.select(root).where(cb.or(firstRestriction,secondRestriction));
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
        Predicate restrictions = cb.equal(root.getAttr("owlClassA").getAttr("stringAttribute"), expected.getOwlClassA().getStringAttribute(),"en");
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
        Predicate restriction = cb.equal(root.getAttr(OWLClassA_.stringAttribute), cb.literal(expected.getStringAttribute(),"en"));
        query.select(root).where(restriction);
        TypedQuery<OWLClassA> tq = getEntityManager().createQuery(query);
        final OWLClassA result = tq.getSingleResult();

        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getStringAttribute(), result.getStringAttribute());
        assertEquals(expected.getTypes(), result.getTypes());
    }
}
