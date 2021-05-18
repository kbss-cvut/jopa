/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.CriteriaQueryImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;

import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.model.query.criteria.Root;
import cz.cvut.kbss.jopa.sessions.CriteriaBuilder;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


public class CriteriaQueryTranslateQueryTest {


    @Mock
    private UnitOfWorkImpl uowMock;

    private static CriteriaBuilder cb;
    private CriteriaParameterFiller criteriaParameterFiller;

    @BeforeAll
    static void init() {
        cb = mock(CriteriaBuilderImpl.class);
    }

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        when(uowMock.useBackupOntologyForQueryProcessing()).thenReturn(Boolean.FALSE);
        when(uowMock.useTransactionalOntologyForQueryProcessing()).thenReturn(Boolean.TRUE);
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        new MetamodelMocks().setMocks(metamodel);
        final MetamodelProvider mpp = mock(MetamodelProvider.class);
        when(uowMock.getMetamodel()).thenReturn(metamodel);
        when(mpp.getMetamodel()).thenReturn(metamodel);
        when(metamodel.getEntities()).thenReturn(Collections.emptySet());
        when(mpp.isEntityType(any())).thenAnswer(inv -> metamodel.isEntityType(inv.getArgument(0)));

        cb = new CriteriaBuilderImpl(uowMock);
        criteriaParameterFiller = new CriteriaParameterFiller();
    }

    @Test
    public void testTranslateQuerySelectAll() {
        CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root);

        final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
    }

    @Test
    public void testTranslateQuerySelectDistinctAll() {
        CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).distinct(true);

        final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT DISTINCT owlclassa FROM OWLClassA owlclassa";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
    }

    @Test
    public void testTranslateQueryCount() {
        CriteriaQuery<Integer> query = cb.createQuery(Integer.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(cb.count(root));

        final String generatedSoqlQuery = ((CriteriaQueryImpl<Integer>) query).translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT COUNT(owlclassa) FROM OWLClassA owlclassa";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
    }

    @Test
    public void testTranslateQueryDistinctCount() {
        CriteriaQuery<Integer> query = cb.createQuery(Integer.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(cb.count(root)).distinct(true);

        final String generatedSoqlQuery = ((CriteriaQueryImpl<Integer>) query).translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT DISTINCT COUNT(owlclassa) FROM OWLClassA owlclassa";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
    }

    @Test
    public void testTranslateQuerySelectAllOrderByEntityDesc() {
        CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).orderBy(cb.desc(root));

        final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa ORDER BY owlclassa DESC";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
    }


    @Nested
    class StringBasedPropertyQueryTests {
        @Test
        public void testTranslateQuerySelectProperty() {
            CriteriaQuery<String> query = cb.createQuery(String.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root.getAttr("stringAttribute"));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<String>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa.stringAttribute FROM OWLClassA owlclassa";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQuerySelectPropertyPath() {
            CriteriaQuery<String> query = cb.createQuery(String.class);
            Root<OWLClassD> root = query.from(OWLClassD.class);
            query.select(root.getAttr("owlClassA").getAttr("stringAttribute"));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<String>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassd.owlClassA.stringAttribute FROM OWLClassD owlclassd";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryEqualRestriction() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.equal(root.getAttr("stringAttribute"), "value"));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNotEqualRestriction() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.notEqual(root.getAttr("stringAttribute"), "value"));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE NOT owlclassa.stringAttribute = :generatedName0";
            //TODO - replace expetedSoqlQuery when SOQL supports equal negation as != or <>
            //final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute != :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterThanRestriction() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.greaterThan(root.getAttr("intAttribute"), 1));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute > :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterThanNegated() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.greaterThan(root.getAttr("intAttribute"), 1).not());

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute <= :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterThanOrEqualRestriction() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.greaterThanOrEqual(root.getAttr("intAttribute"), 1));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute >= :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterOrEqualNegated() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.greaterThanOrEqual(root.getAttr("intAttribute"), 1).not());

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute < :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryLessThanRestriction() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.lessThan(root.getAttr("intAttribute"), 1));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute < :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryLessThanNegated() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.lessThan(root.getAttr("intAttribute"), 1).not());

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute >= :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryLessOrEqualRestriction() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.lessThanOrEqual(root.getAttr("intAttribute"), 1));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute <= :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryLessOrEqualNegated() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.lessThanOrEqual(root.getAttr("intAttribute"), 1).not());

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute > :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNegatedRestriction() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.equal(root.getAttr("stringAttribute"), "value").not());

            final CriteriaQueryImpl<OWLClassA> criteriaQuery = (CriteriaQueryImpl<OWLClassA>) query;
            final String generatedSoqlQuery = criteriaQuery.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE NOT owlclassa.stringAttribute = :generatedName0";
            //TODO - replace expetedSoqlQuery when SOQL supports equal negation as != or <>
            //final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute != :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryMultipleOrRestrictions() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            Predicate restrictions = cb.or(
                    cb.equal(root.getAttr("stringAttribute"), "valueOne"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTwo"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTree")
            );
            query.select(root).where(restrictions);

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0 OR owlclassa.stringAttribute = :generatedName1 OR owlclassa.stringAttribute = :generatedName2";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryMultipleAndRestrictions() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            Predicate restrictions = cb.and(
                    cb.equal(root.getAttr("intAttribute"), 0),
                    cb.equal(root.getAttr("doubleAttribute"), 1.1),
                    cb.equal(root.getAttr("booleanAttribute"), true)
            );
            query.select(root).where(restrictions);

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute = :generatedName0 AND owlclassm.doubleAttribute = :generatedName1 AND owlclassm.booleanAttribute = :generatedName2";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }


        @Test
        public void testTranslateQueryMultipleOrAndRestrictions() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.or(
                    cb.and(
                            cb.equal(root.getAttr("doubleAttribute"), 1.1),
                            cb.equal(root.getAttr("intAttribute"), 1)
                    ),
                    cb.and(
                            cb.equal(root.getAttr("doubleAttribute"), 2.2),
                            cb.equal(root.getAttr("intAttribute"), 2)
                    ))
            );

            final CriteriaQueryImpl<OWLClassM> criteriaQuery = (CriteriaQueryImpl<OWLClassM>) query;
            final String generatedSoqlQuery = criteriaQuery.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE (owlclassm.doubleAttribute = :generatedName0 AND owlclassm.intAttribute = :generatedName1) OR (owlclassm.doubleAttribute = :generatedName2 AND owlclassm.intAttribute = :generatedName3)";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNegatedMultipleRestrictionsByNotMethod() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            Predicate restrictions = cb.or(
                    cb.equal(root.getAttr("stringAttribute"), "valueOne"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTwo"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTree")
            );
            query.select(root).where(restrictions.not());

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE NOT owlclassa.stringAttribute = :generatedName0 AND NOT owlclassa.stringAttribute = :generatedName1 AND NOT owlclassa.stringAttribute = :generatedName2";
            //TODO - replace expetedSoqlQuery when SOQL supports equal negation as != or <>
            //final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute != :generatedName0 AND owlclassa.stringAttribute != :generatedName1 AND owlclassa.stringAttribute != :generatedName2";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNegatedMultipleRestrictionsByFactory() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            Predicate restrictions = cb.or(
                    cb.equal(root.getAttr("stringAttribute"), "valueOne"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTwo"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTree")
            );
            query.select(root).where(cb.not(restrictions));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE NOT owlclassa.stringAttribute = :generatedName0 AND NOT owlclassa.stringAttribute = :generatedName1 AND NOT owlclassa.stringAttribute = :generatedName2";
            //TODO - replace expetedSoqlQuery when SOQL supports equal negation as != or <>
            //final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute != :generatedName0 AND owlclassa.stringAttribute != :generatedName1 AND owlclassa.stringAttribute != :generatedName2";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryLessThanAndGreaterEqualOrNegatedGreaterThanAndEqual() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.or(
                    cb.and(
                            cb.lessThan(root.getAttr("doubleAttribute"), 1.1),
                            cb.greaterThanOrEqual(root.getAttr("intAttribute"), 1)
                    ),
                    cb.and(
                            cb.greaterThan(root.getAttr("doubleAttribute"), 2.2),
                            cb.equal(root.getAttr("intAttribute"), 2)
                    ).not())
            );

            final CriteriaQueryImpl<OWLClassM> criteriaQuery = (CriteriaQueryImpl<OWLClassM>) query;
            final String generatedSoqlQuery = criteriaQuery.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE (owlclassm.doubleAttribute < :generatedName0 AND owlclassm.intAttribute >= :generatedName1) OR (owlclassm.doubleAttribute <= :generatedName2 OR NOT owlclassm.intAttribute = :generatedName3)";
            //TODO - replace expetedSoqlQuery when SOQL supports equal negation as != or <>
            //final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE (owlclassm.doubleAttribute < :generatedName0 AND owlclassm.intAttribute >= :generatedName1) OR (owlclassm.doubleAttribute <= :generatedName2 OR owlclassm.intAttribute != :generatedName3)";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }


        @Test
        public void testTranslateQueryLikeRestriction() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.like(root.getAttr("stringAttribute"), "pattern"));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute LIKE :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNotLikeRestriction() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.notLike(root.getAttr("stringAttribute"), "pattern"));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute NOT LIKE :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryInRestriction() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.in(root.getAttr("stringAttribute")).value("value").value("anotherValue"));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute IN(:generatedName0, :generatedName1)";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryInNegatedRestriction() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.not(cb.in(root.getAttr("stringAttribute")).value("value").value("anotherValue")));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute NOT IN(:generatedName0, :generatedName1)";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNotInRestriction() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.notIn(root.getAttr("stringAttribute")).value("value").value("anotherValue"));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute NOT IN(:generatedName0, :generatedName1)";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryOrderByOnePropertyDesc() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).orderBy(cb.desc(root.getAttr("stringAttribute")));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa ORDER BY owlclassa.stringAttribute DESC";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryOrderByTwoPropertiesAscAndDesc() {
            CriteriaQuery<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).orderBy(
                    cb.asc(root.getAttr("intAttribute")),
                    cb.desc(root.getAttr("doubleAttribute"))
            );

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm ORDER BY owlclassm.intAttribute ASC, owlclassm.doubleAttribute DESC";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }
    }

    @Nested
    class MetamodelBasedPropertyQueryTests {
        @Test
        public void testTranslateQuerySelectProperty() {
            CriteriaQuery<String> query = cb.createQuery(String.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root.getAttr(OWLClassA_.stringAttribute));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<String>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa.stringAttribute FROM OWLClassA owlclassa";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQuerySelectPropertyPath() {
        CriteriaQuery<String> query = cb.createQuery(String.class);
        Root<OWLClassD> root = query.from(OWLClassD.class);
        query.select(root.getAttr(OWLClassD_.owlClassA).getAttr(OWLClassA_.stringAttribute));

        final String generatedSoqlQuery = ((CriteriaQueryImpl<String>) query).translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT owlclassd.owlClassA.stringAttribute FROM OWLClassD owlclassd";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryRestrictionWithLiteral() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.equal(root.getAttr(OWLClassA_.stringAttribute),cb.literal("value")));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryRestrictionWithParameterExpression() {
            CriteriaQuery<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            ParameterExpression<String> parameter = cb.parameter(String.class);
            query.select(root).where(cb.equal(root.getAttr(OWLClassA_.stringAttribute),parameter));

            final String generatedSoqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

    }
}
