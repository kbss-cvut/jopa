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
import cz.cvut.kbss.jopa.sessions.CriteriaFactory;
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

    private static CriteriaFactory f;
    private CriteriaParameterFiller criteriaParameterFiller;

    @BeforeAll
    static void init() {
        f = mock(CriteriaFactoryImpl.class);
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

        f = new CriteriaFactoryImpl(uowMock);
        criteriaParameterFiller = new CriteriaParameterFiller();
    }

    @Test
    public void testTranslateQuerySelectAll() {
        CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root);

        final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
        final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa";
        assertEquals(expectedJpqlQuery, generatedJpqlQuery);
    }

    @Test
    public void testTranslateQuerySelectDistinctAll() {
        CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).distinct(true);

        final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
        final String expectedJpqlQuery = "SELECT DISTINCT owlclassa FROM OWLClassA owlclassa";
        assertEquals(expectedJpqlQuery, generatedJpqlQuery);
    }

    @Test
    public void testTranslateQueryCount() {
        CriteriaQuery<Integer> query = f.createQuery(Integer.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(f.count(root));

        final String generatedJpqlQuery = ((CriteriaQueryImpl<Integer>) query).translateQuery(criteriaParameterFiller);
        final String expectedJpqlQuery = "SELECT COUNT(owlclassa) FROM OWLClassA owlclassa";
        assertEquals(expectedJpqlQuery, generatedJpqlQuery);
    }

    @Test
    public void testTranslateQueryDistinctCount() {
        CriteriaQuery<Integer> query = f.createQuery(Integer.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(f.count(root)).distinct(true);

        final String generatedJpqlQuery = ((CriteriaQueryImpl<Integer>) query).translateQuery(criteriaParameterFiller);
        final String expectedJpqlQuery = "SELECT DISTINCT COUNT(owlclassa) FROM OWLClassA owlclassa";
        assertEquals(expectedJpqlQuery, generatedJpqlQuery);
    }

    @Test
    public void testTranslateQuerySelectAllOrderByEntityDesc() {
        CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        query.select(root).orderBy(f.desc(root));

        final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
        final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa ORDER BY owlclassa DESC";
        assertEquals(expectedJpqlQuery, generatedJpqlQuery);
    }


    @Nested
    class StringBasedPropertyQueryTests {
        @Test
        public void testTranslateQuerySelectProperty() {
            CriteriaQuery<String> query = f.createQuery(String.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root.getAttr("stringAttribute"));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<String>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa.stringAttribute FROM OWLClassA owlclassa";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQuerySelectPropertyPath() {
            CriteriaQuery<String> query = f.createQuery(String.class);
            Root<OWLClassD> root = query.from(OWLClassD.class);
            query.select(root.getAttr("owlClassA").getAttr("stringAttribute"));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<String>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassd.owlClassA.stringAttribute FROM OWLClassD owlclassd";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryEqualRestriction() {
            CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(f.equal(root.getAttr("stringAttribute"), "value"));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryNotEqualRestriction() {
            CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(f.notEqual(root.getAttr("stringAttribute"), "value"));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute <> :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterThanRestriction() {
            CriteriaQuery<OWLClassM> query = f.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(f.greaterThan(root.getAttr("intAttribute"), 1));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute > :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterThanNegated() {
            CriteriaQuery<OWLClassM> query = f.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(f.greaterThan(root.getAttr("intAttribute"), 1).not());

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute <= :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterThanOrEqualRestriction() {
            CriteriaQuery<OWLClassM> query = f.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(f.greaterThanOrEqual(root.getAttr("intAttribute"), 1));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute >= :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryGreaterOrEqualNegated() {
            CriteriaQuery<OWLClassM> query = f.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(f.greaterThanOrEqual(root.getAttr("intAttribute"), 1).not());

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute < :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryLessThanRestriction() {
            CriteriaQuery<OWLClassM> query = f.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(f.lessThan(root.getAttr("intAttribute"), 1));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute < :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryLessThanNegated() {
            CriteriaQuery<OWLClassM> query = f.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(f.lessThan(root.getAttr("intAttribute"), 1).not());

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute >= :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryLessOrEqualRestriction() {
            CriteriaQuery<OWLClassM> query = f.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(f.lessThanOrEqual(root.getAttr("intAttribute"), 1));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute <= :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryLessOrEqualNegated() {
            CriteriaQuery<OWLClassM> query = f.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(f.lessThanOrEqual(root.getAttr("intAttribute"), 1).not());

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute > :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryNegatedRestriction() {
            CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(f.equal(root.getAttr("stringAttribute"), "value").not());

            final CriteriaQueryImpl<OWLClassA> criteriaQuery = (CriteriaQueryImpl<OWLClassA>) query;
            final String generatedJpqlQuery = criteriaQuery.translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute <> :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryMultipleOrRestrictions() {
            CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            Predicate restrictions = f.or(
                    f.equal(root.getAttr("stringAttribute"), "valueOne"),
                    f.equal(root.getAttr("stringAttribute"), "valueTwo"),
                    f.equal(root.getAttr("stringAttribute"), "valueTree")
            );
            query.select(root).where(restrictions);

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0 OR owlclassa.stringAttribute = :generatedName1 OR owlclassa.stringAttribute = :generatedName2";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryMultipleAndRestrictions() {
            CriteriaQuery<OWLClassM> query = f.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            Predicate restrictions = f.and(
                    f.equal(root.getAttr("intAttribute"), 0),
                    f.equal(root.getAttr("doubleAttribute"), 1.1),
                    f.equal(root.getAttr("booleanAttribute"), true)
            );
            query.select(root).where(restrictions);

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE owlclassm.intAttribute = :generatedName0 AND owlclassm.doubleAttribute = :generatedName1 AND owlclassm.booleanAttribute = :generatedName2";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }


        @Test
        public void testTranslateQueryMultipleOrAndRestrictions() {
            CriteriaQuery<OWLClassM> query = f.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).where(f.or(
                    f.and(
                            f.equal(root.getAttr("doubleAttribute"), 1.1),
                            f.equal(root.getAttr("intAttribute"), 1)
                    ),
                    f.and(
                            f.equal(root.getAttr("doubleAttribute"), 2.2),
                            f.equal(root.getAttr("intAttribute"), 2)
                    ))
            );

            final CriteriaQueryImpl<OWLClassM> criteriaQuery = (CriteriaQueryImpl<OWLClassM>) query;
            final String generatedJpqlQuery = criteriaQuery.translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm WHERE (owlclassm.doubleAttribute = :generatedName0 AND owlclassm.intAttribute = :generatedName1) OR (owlclassm.doubleAttribute = :generatedName2 AND owlclassm.intAttribute = :generatedName3)";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryNegatedMultipleRestrictionsByNotMethod() {
            CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            Predicate restrictions = f.or(
                    f.equal(root.getAttr("stringAttribute"), "value"),
                    f.equal(root.getAttr("stringAttribute"), "value"),
                    f.equal(root.getAttr("stringAttribute"), "value")
            );
            query.select(root).where(restrictions.not());

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute <> :generatedName0 AND owlclassa.stringAttribute <> :generatedName1 AND owlclassa.stringAttribute <> :generatedName2";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryNegatedMultipleRestrictionsByFactory() {
            CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            Predicate restrictions = f.or(
                    f.equal(root.getAttr("stringAttribute"), "value"),
                    f.equal(root.getAttr("stringAttribute"), "value"),
                    f.equal(root.getAttr("stringAttribute"), "value")
            );
            query.select(root).where(f.not(restrictions));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute <> :generatedName0 AND owlclassa.stringAttribute <> :generatedName1 AND owlclassa.stringAttribute <> :generatedName2";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }


        @Test
        public void testTranslateQueryLikeRestriction() {
            CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(f.like(root.getAttr("stringAttribute"), "pattern"));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute LIKE :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryNotLikeRestriction() {
            CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(f.notLike(root.getAttr("stringAttribute"), "pattern"));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute NOT LIKE :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryOrderByOnePropertyDesc() {
            CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).orderBy(f.desc(root.getAttr("stringAttribute")));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa ORDER BY owlclassa.stringAttribute DESC";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryOrderByTwoPropertiesAscAndDesc() {
            CriteriaQuery<OWLClassM> query = f.createQuery(OWLClassM.class);
            Root<OWLClassM> root = query.from(OWLClassM.class);
            query.select(root).orderBy(
                    f.asc(root.getAttr("intAttribute")),
                    f.desc(root.getAttr("doubleAttribute"))
            );

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassM>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassm FROM OWLClassM owlclassm ORDER BY owlclassm.intAttribute ASC, owlclassm.doubleAttribute DESC";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }
    }

    @Nested
    class MetamodelBasedPropertyQueryTests {
        @Test
        public void testTranslateQuerySelectProperty() {
            CriteriaQuery<String> query = f.createQuery(String.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root.getAttr(OWLClassA_.stringAttribute));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<String>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa.stringAttribute FROM OWLClassA owlclassa";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQuerySelectPropertyPath() {
        CriteriaQuery<String> query = f.createQuery(String.class);
        Root<OWLClassD> root = query.from(OWLClassD.class);
        query.select(root.getAttr(OWLClassD_.owlClassA).getAttr(OWLClassA_.stringAttribute));

        final String generatedJpqlQuery = ((CriteriaQueryImpl<String>) query).translateQuery(criteriaParameterFiller);
        final String expectedJpqlQuery = "SELECT owlclassd.owlClassA.stringAttribute FROM OWLClassD owlclassd";
        assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryRestrictionWithLiteral() {
            CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            query.select(root).where(f.equal(root.getAttr(OWLClassA_.stringAttribute),f.literal("value")));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

        @Test
        public void testTranslateQueryRestrictionWithParameterExpression() {
            CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
            Root<OWLClassA> root = query.from(OWLClassA.class);
            ParameterExpression<String> parameter = f.parameter(String.class);
            query.select(root).where(f.equal(root.getAttr(OWLClassA_.stringAttribute),parameter));

            final String generatedJpqlQuery = ((CriteriaQueryImpl<OWLClassA>) query).translateQuery(criteriaParameterFiller);
            final String expectedJpqlQuery = "SELECT owlclassa FROM OWLClassA owlclassa WHERE owlclassa.stringAttribute = :generatedName0";
            assertEquals(expectedJpqlQuery, generatedJpqlQuery);
        }

    }


    //    @Test
//    public void testParseFindByMultipleAndAndQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND p.gender = :gender AND p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/gender> ?gender . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }

//
//    @Test
//    public void testParseFindOneLikeQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username LIKE :username";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?pUsername . FILTER (regex(?pUsername, ?username) ) }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(3, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindOneJoinedQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(3, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindMutipleJoinedQuery() {
//        final String jpqlQuery = "SELECT a FROM OWLClassA a WHERE a.OWLClassB.OWLClassC.OWLClassD = :d";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/OWLClassA> . ?x <http://www.example.org/OWLClassB> ?OWLClassB . ?OWLClassB <http://www.example.org/OWLClassC> ?OWLClassC . ?OWLClassC <http://www.example.org/OWLClassD> ?d . }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(4, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindMutipleJoinedQueryFilter() {
//        final String jpqlQuery = "SELECT a FROM OWLClassA a WHERE a.OWLClassB.OWLClassC.OWLClassD.intAttribute > :intAttribute";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/OWLClassA> . ?x <http://www.example.org/OWLClassB> ?OWLClassB . ?OWLClassB <http://www.example.org/OWLClassC> ?OWLClassC . ?OWLClassC <http://www.example.org/OWLClassD> ?OWLClassD . ?OWLClassD <http://www.example.org/intAttribute> ?aOWLClassBOWLClassCOWLClassDIntAttribute . FILTER (?aOWLClassBOWLClassCOWLClassDIntAttribute > ?intAttribute) }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(6, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindOneOrderByQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age ORDER BY p.age DESC";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } ORDER BY DESC(?pAge) ";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(3, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindOneOrderByNotInWhereQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age ORDER BY p.username DESC";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } ORDER BY DESC(?username) ";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(4, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindOneGroupByQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age GROUP BY p.age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } GROUP BY ?pAge ";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(3, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindOneGroupByNotInWhereQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age GROUP BY p.gender";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/gender> ?gender . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } GROUP BY ?gender ";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(4, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByOneNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(2, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleAndQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(4, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotAndQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(4, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleAndNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(4, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotAndNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(4, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleOrQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber OR p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotOrQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(4, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleOrNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username OR NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(4, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotOrNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(4, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleOrOrderByNotInWhereQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age OR p.gender = :gender ORDER BY p.username DESC";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } UNION { ?x <http://www.example.org/gender> ?gender . } } ORDER BY DESC(?username) ";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleOrGroupByNotInWhereQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age OR p.gender = :gender GROUP BY p.username DESC";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } UNION { ?x <http://www.example.org/gender> ?gender . } } GROUP BY ?username ";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleAndOrQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber AND p.gender = :gender OR p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . ?x <http://www.example.org/gender> ?gender . } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(6, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotAndOrQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.phone.number = :phoneNumber AND p.gender = :gender OR p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/gender> ?gender . FILTER NOT EXISTS { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(6, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleAndNotOrQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber AND NOT p.gender = :gender OR p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(6, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleAndOrNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber AND p.gender = :gender OR NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . ?x <http://www.example.org/gender> ?gender . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(6, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotAndNotOrQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.gender = :gender OR p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/gender> ?gender . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleAndNotOrNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND NOT p.gender = :gender OR NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotAndOrNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND p.gender = :gender OR NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/gender> ?gender . FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleAndAndQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND p.gender = :gender AND p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/gender> ?gender . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotAndAndQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND p.gender = :gender AND p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/gender> ?gender . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleAndNotAndQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND NOT p.gender = :gender AND p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleAndAndNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND p.gender = :gender AND NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/gender> ?gender . FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotAndNotAndQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.gender = :gender AND p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/gender> ?gender . } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleAndNotAndNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND NOT p.gender = :gender AND NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotAndAndNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND p.gender = :gender AND NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/gender> ?gender . FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotAndNotAndNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.gender = :gender AND NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/gender> ?gender . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleOrOrQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username OR p.gender = :gender OR p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . } UNION { ?x <http://www.example.org/gender> ?gender . } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotOrOrQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR p.gender = :gender OR p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { ?x <http://www.example.org/gender> ?gender . } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleOrNotOrQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username OR NOT p.gender = :gender OR p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleOrOrNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username OR p.gender = :gender OR NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . } UNION { ?x <http://www.example.org/gender> ?gender . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotOrNotOrQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR NOT p.gender = :gender OR p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleOrNotOrNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username OR NOT p.gender = :gender OR NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotOrOrNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR p.gender = :gender OR NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { ?x <http://www.example.org/gender> ?gender . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
//
//    @Test
//    public void testParseFindByMultipleNotOrNotOrNotQuery() {
//        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR NOT p.gender = :gender OR NOT p.age > :age";
//        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
//        final QueryHolder holder = sut.parseQuery(jpqlQuery);
//        assertEquals(expectedSparqlQuery, holder.getQuery());
//        assertEquals(5, holder.getParameters().size());
//    }
}