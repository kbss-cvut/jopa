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
package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassA_;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassD_;
import cz.cvut.kbss.jopa.environment.OWLClassF;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.CriteriaQueryImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.query.criteria.ParameterExpression;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.model.query.criteria.Root;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


@Execution(ExecutionMode.CONCURRENT)
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class CriteriaQueryTranslateQueryTest {

    @Mock
    private UnitOfWork uowMock;

    private CriteriaBuilderImpl cb;
    private CriteriaParameterFiller criteriaParameterFiller;

    @BeforeEach
    void setUp() throws Exception {
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        new MetamodelMocks().setMocks(metamodel);
        final MetamodelProvider mpp = mock(MetamodelProvider.class);
        when(uowMock.getMetamodel()).thenReturn(metamodel);
        when(mpp.getMetamodel()).thenReturn(metamodel);
        when(metamodel.getEntities()).thenReturn(Collections.emptySet());
        when(mpp.isEntityType(any())).thenAnswer(inv -> metamodel.isEntityType(inv.getArgument(0)));

        this.cb = new CriteriaBuilderImpl(metamodel);
        this.criteriaParameterFiller = new CriteriaParameterFiller();
    }

    @Test
    public void testTranslateQuerySelectAll() {
        CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root);

        final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
    }

    @Test
    public void testTranslateQuerySelectDistinctAll() {
        CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).distinct(true);

        final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT DISTINCT owlclassa FROM OWLClassA owlclassa";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
    }

    @Test
    public void testTranslateQueryCount() {
        CriteriaQueryImpl<Integer> query = cb.createQuery(Integer.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(cb.count(root));

        final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT COUNT(owlclassa) FROM OWLClassA owlclassa";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
    }

    @Test
    public void testTranslateQueryDistinctCount() {
        CriteriaQueryImpl<Integer> query = cb.createQuery(Integer.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(cb.count(root)).distinct(true);

        final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT DISTINCT COUNT(owlclassa) FROM OWLClassA owlclassa";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
    }

    @Test
    public void testTranslateQuerySelectAllOrderByEntityDesc() {
        CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).orderBy(cb.desc(root));

        final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa ORDER BY owlclassa DESC";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
    }

    @Test
    void translateQuerySupportsLangMatches() {
        CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).where(cb.langMatches(root.getAttr("stringAttribute"), cb.parameter(String.class)));

        final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
        final String expectedSoqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE LANGMATCHES(owlclassa.stringAttribute, :generatedName0)";
        assertEquals(expectedSoqlQuery, generatedSoqlQuery);
    }

    @Nested
    class StringBasedPropertyQueryTests {
        @Test
        public void testTranslateQuerySelectProperty() {
            CriteriaQueryImpl<String> query = cb.createQuery(String.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root.getAttr("stringAttribute"));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa.stringAttribute FROM OWLClassA owlclassa";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        void testTranslateQueryCountProperty() {
            CriteriaQueryImpl<Integer> query = cb.createQuery(Integer.class);
            Root<OWLClassD> root = query.from(OWLClassD.class);
            query.select(cb.count(root.getAttr("owlClassA")));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT COUNT(owlclassd.owlClassA) FROM OWLClassD owlclassd";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        void testTranslateQueryDistinctCountProperty() {
            CriteriaQueryImpl<Integer> query = cb.createQuery(Integer.class);
            Root<OWLClassD> root = query.from(OWLClassD.class);
            query.select(cb.count(root.getAttr("owlClassA"))).distinct();

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT DISTINCT COUNT(owlclassd.owlClassA) FROM OWLClassD owlclassd";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        void parseQuerySupportsCountWithProjectedAttribute() {
            CriteriaQueryImpl<Integer> query = cb.createQuery(Integer.class);
            Root<OWLClassD> root = query.from(OWLClassD.class);
            query.select(cb.count(root.getAttr("owlClassA"))).distinct();

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT DISTINCT COUNT(owlclassd.owlClassA) FROM OWLClassD owlclassd";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        void parseQuerySupportsCountWithProjectedAttributeAndRelatedAttribute() {
            CriteriaQueryImpl<Integer> query = cb.createQuery(Integer.class);
            Root<OWLClassD> root = query.from(OWLClassD.class);
            query.select(cb.count(root.getAttr("owlClassA"))).distinct()
                 .where(cb.equal(root.getAttr("owlClassA").getAttr("stringAttribute"), ""));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT DISTINCT COUNT(owlclassd.owlClassA) FROM OWLClassD owlclassd WHERE owlclassd.owlClassA.stringAttribute = :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQuerySelectPropertyPath() {
            CriteriaQueryImpl<String> query = cb.createQuery(String.class);
            Root<OWLClassD> root = query.from(OWLClassD.class);
            query.select(root.getAttr("owlClassA").getAttr("stringAttribute"));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassd.owlClassA.stringAttribute FROM OWLClassD owlclassd";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryEqualRestriction() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.equal(root.getAttr("stringAttribute"), "value"));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNotEqualRestriction() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.notEqual(root.getAttr("stringAttribute"), "value"));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute <> :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterThanRestriction() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.greaterThan(root.getAttr("intAttribute"), 1));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute > :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterThanNegated() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.greaterThan(root.getAttr("intAttribute"), 1).not());

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute <= :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterThanOrEqualRestriction() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.greaterThanOrEqual(root.getAttr("intAttribute"), 1));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute >= :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterOrEqualNegated() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.greaterThanOrEqual(root.getAttr("intAttribute"), 1).not());

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute < :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryLessThanRestriction() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.lessThan(root.getAttr("intAttribute"), 1));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute < :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryLessThanNegated() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.lessThan(root.getAttr("intAttribute"), 1).not());

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute >= :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryLessOrEqualRestriction() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.lessThanOrEqual(root.getAttr("intAttribute"), 1));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute <= :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryLessOrEqualNegated() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(cb.lessThanOrEqual(root.getAttr("intAttribute"), 1).not());

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute > :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNegatedRestriction() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.equal(root.getAttr("stringAttribute"), "value").not());

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute <> :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryMultipleOrRestrictions() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            Predicate restrictions = cb.or(
                    cb.equal(root.getAttr("stringAttribute"), "valueOne"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTwo"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTree")
            );
            query.select(root).where(restrictions);

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0 OR owlclassa.stringAttribute = :generatedName1 OR owlclassa.stringAttribute = :generatedName2";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryMultipleAndRestrictions() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            Predicate restrictions = cb.and(
                    cb.equal(root.getAttr("intAttribute"), 0),
                    cb.equal(root.getAttr("doubleAttribute"), 1.1),
                    cb.equal(root.getAttr("booleanAttribute"), true)
            );
            query.select(root).where(restrictions);

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute = :generatedName0 AND owlclassm.doubleAttribute = :generatedName1 AND owlclassm.booleanAttribute = :generatedName2";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryMultipleRestrictionsWithList() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            List<Predicate> restrictions = new ArrayList<>();
            restrictions.add(cb.equal(root.getAttr("intAttribute"), 0));
            restrictions.add(cb.equal(root.getAttr("doubleAttribute"), 1.1));
            restrictions.add(cb.equal(root.getAttr("booleanAttribute"), true));
            query.select(root).where(restrictions);

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute = :generatedName0 AND owlclassm.doubleAttribute = :generatedName1 AND owlclassm.booleanAttribute = :generatedName2";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }


        @Test
        public void testTranslateQueryMultipleOrAndRestrictions() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
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

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE (owlclassm.doubleAttribute = :generatedName0 AND owlclassm.intAttribute = :generatedName1) OR (owlclassm.doubleAttribute = :generatedName2 AND owlclassm.intAttribute = :generatedName3)";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNegatedMultipleRestrictionsByNotMethod() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            Predicate restrictions = cb.or(
                    cb.equal(root.getAttr("stringAttribute"), "valueOne"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTwo"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTree")
            );
            query.select(root).where(restrictions.not());

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute <> :generatedName0 AND owlclassa.stringAttribute <> :generatedName1 AND owlclassa.stringAttribute <> :generatedName2";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNegatedMultipleRestrictionsByFactory() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            Predicate restrictions = cb.or(
                    cb.equal(root.getAttr("stringAttribute"), "valueOne"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTwo"),
                    cb.equal(root.getAttr("stringAttribute"), "valueTree")
            );
            query.select(root).where(cb.not(restrictions));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute <> :generatedName0 AND owlclassa.stringAttribute <> :generatedName1 AND owlclassa.stringAttribute <> :generatedName2";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryLessThanAndGreaterEqualOrNegatedGreaterThanAndEqual() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
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

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm WHERE (owlclassm.doubleAttribute < :generatedName0 AND owlclassm.intAttribute >= :generatedName1) OR (owlclassm.doubleAttribute <= :generatedName2 OR owlclassm.intAttribute <> :generatedName3)";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryLikeRestriction() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.like(root.getAttr("stringAttribute"), "pattern"));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute LIKE :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNotLikeRestriction() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.notLike(root.getAttr("stringAttribute"), "pattern"));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute NOT LIKE :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryInRestriction() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.in(root.getAttr("stringAttribute")).value("value").value("anotherValue"));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute IN (:generatedName0)";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryInNegatedRestriction() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root)
                 .where(cb.not(cb.in(root.getAttr("stringAttribute")).value("value").value("anotherValue")));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute NOT IN (:generatedName0)";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryNotInRestriction() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.notIn(root.getAttr("stringAttribute")).value("value").value("anotherValue"));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute NOT IN (:generatedName0)";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryOrderByOnePropertyDesc() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).orderBy(cb.desc(root.getAttr("stringAttribute")));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa ORDER BY owlclassa.stringAttribute DESC";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryOrderByTwoPropertiesAscAndDesc() {
            CriteriaQueryImpl<OWLClassM> query = cb.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).orderBy(
                    cb.asc(root.getAttr("intAttribute")),
                    cb.desc(root.getAttr("doubleAttribute"))
            );

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassm FROM OWLClassM owlclassm ORDER BY owlclassm.intAttribute ASC, owlclassm.doubleAttribute DESC";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        void translateQueryTranslatesLangMatching() {
            final CriteriaQueryImpl<OWLClassU> query = cb.createQuery(OWLClassU.class);
            final Root<OWLClassU> root = query.from(OWLClassU.class);
            query.select(root).where(cb.equal(cb.lang(root.getAttr("singularStringAtt")), "en"));

            final String result = query.translateQuery(criteriaParameterFiller);
            final String expectedSoql = "SELECT owlclassu FROM OWLClassU owlclassu WHERE LANG(owlclassu.singularStringAtt) = :generatedName0";
            assertEquals(expectedSoql, result);
        }

        @Test
        void translateQueryTranslatesIsMemberWithLiteralToMemberOfExpression() {
            final CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            final Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.isMember(Generators.createPropertyIdentifier(), root.getAttr("types")));
            final String expectedSoql = "SELECT owlclassa FROM OWLClassA owlclassa WHERE :generatedName0 MEMBER OF owlclassa.types";
            assertEquals(expectedSoql, query.translateQuery(criteriaParameterFiller));
        }

        @Test
        void translateQueryTranslatesIsNotMemberWithLiteralToNotMemberOfExpression() {
            final CriteriaQueryImpl<OWLClassF> query = cb.createQuery(OWLClassF.class);
            final Root<OWLClassF> root = query.from(OWLClassF.class);
            query.select(root).where(cb.isNotMember(Generators.createPropertyIdentifier(), root.getAttr("simpleSet")));
            final String expectedSoql = "SELECT owlclassf FROM OWLClassF owlclassf WHERE :generatedName0 NOT MEMBER OF owlclassf.simpleSet";
            assertEquals(expectedSoql, query.translateQuery(criteriaParameterFiller));
        }

        @Test
        void translateQueryTranslatesMultipleIsMemberRestrictionsForRdfContainer() {
            final CriteriaQueryImpl<OWLClassC> query = cb.createQuery(OWLClassC.class);
            final Root<OWLClassC> root = query.from(OWLClassC.class);
            final List<URI> values = List.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier());
            final List<Predicate> predicates = values.stream().map(elem -> cb.isMember(elem, root.getAttr("rdfSeq")))
                                                     .toList();
            query.select(root).where(predicates.toArray(new Predicate[0]));
            final String expectedSoql = "SELECT owlclassc FROM OWLClassC owlclassc WHERE :generatedName0 MEMBER OF owlclassc.rdfSeq AND :generatedName1 MEMBER OF owlclassc.rdfSeq";
            assertEquals(expectedSoql, query.translateQuery(criteriaParameterFiller));
        }
    }

    @Nested
    class MetamodelBasedPropertyQueryTests {
        @Test
        public void testTranslateQuerySelectProperty() {
            CriteriaQueryImpl<String> query = cb.createQuery(String.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root.getAttr(OWLClassA_.stringAttribute));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassa.stringAttribute FROM OWLClassA owlclassa";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQuerySelectPropertyPath() {
            CriteriaQueryImpl<String> query = cb.createQuery(String.class);
            Root<OWLClassD> root = query.from(OWLClassD.class);
            query.select(root.getAttr(OWLClassD_.owlClassA).getAttr(OWLClassA_.stringAttribute));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery = "SELECT owlclassd.owlClassA.stringAttribute FROM OWLClassD owlclassd";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryRestrictionWithLiteral() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(cb.equal(root.getAttr(OWLClassA_.stringAttribute), cb.literal("value")));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }

        @Test
        public void testTranslateQueryRestrictionWithParameterExpression() {
            CriteriaQueryImpl<OWLClassA> query = cb.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            ParameterExpression<String> parameter = cb.parameter(String.class);
            query.select(root).where(cb.equal(root.getAttr(OWLClassA_.stringAttribute), parameter));

            final String generatedSoqlQuery = query.translateQuery(criteriaParameterFiller);
            final String expectedSoqlQuery =
                    "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0";
            assertEquals(expectedSoqlQuery, generatedSoqlQuery);
        }
    }
}
