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
package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.environment.Person;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exception.SoqlException;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.sparql.Sparql11QueryParser;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.matchesPattern;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SoqlQueryParserTest {

    @Mock
    private MetamodelImpl metamodel;

    private QueryParser sut;

    @BeforeEach
    void setUp() throws Exception {
        MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodel);
        final UnitOfWork uow = mock(UnitOfWork.class);
        when(uow.getConfiguration()).thenReturn(new Configuration());
        when(uow.getMetamodel()).thenReturn(metamodel);
        when(uow.isEntityType(any())).thenAnswer(inv -> metamodel.isEntityType(inv.getArgument(0)));
        final QueryParser qp = new Sparql11QueryParser(new ParameterValueFactory(uow));
        this.sut = new SoqlQueryParser(qp, metamodel);
    }

    @Test
    public void testParseFindAllQuery() {
        final String soqlQuery = "SELECT p FROM Person p";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(1, holder.getParameters().size());
    }

    @Test
    public void testParseDistinctFindAllQuery() {
        final String soqlQuery = "SELECT DISTINCT p FROM Person p";
        final String expectedSparqlQuery =
                "SELECT DISTINCT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(1, holder.getParameters().size());
    }

    @Test
    public void testParseCountQuery() {
        final String soqlQuery = "SELECT COUNT(p) FROM Person p";
        final String expectedSparqlQuery =
                "SELECT (COUNT(?x) AS ?count) WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(2, holder.getParameters().size());
    }

    @Test
    public void testParseDistinctCountQuery() {
        final String soqlQuery = "SELECT DISTINCT COUNT(p) FROM Person p";
        final String expectedSparqlQuery =
                "SELECT (COUNT(DISTINCT ?x) AS ?count) WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(2, holder.getParameters().size());
    }

    @Test
    public void testParseFindAllOWLClassAQuery() {
        final String soqlQuery = "SELECT a FROM OWLClassA a";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <" + Vocabulary.c_OwlClassA + "> . }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(1, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.username = :uname";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?uname . }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(2, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneLikeQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.username LIKE :username";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?pUsername . FILTER (REGEX(?pUsername, ?username)) }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneJoinedQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x <" + Vocabulary.p_p_hasPhone + "> ?phone . ?phone <" + Vocabulary.p_p_phoneNumber + "> ?phoneNumber . }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindMultipleJoinedQuery() {
        final String soqlQuery = "SELECT g FROM OWLClassG g WHERE g.owlClassH.owlClassA.stringAttribute = :d";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a <" + Vocabulary.c_OwlClassG + "> . ?x <" + Vocabulary.p_g_hasH + "> ?owlClassH . ?owlClassH <" + Vocabulary.p_h_hasA + "> ?owlClassA . ?owlClassA <" + Vocabulary.p_a_stringAttribute + "> ?d . }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindMultipleJoinedQueryFilter() {
        final String soqlQuery = "SELECT g FROM OWLClassG g WHERE g.owlClassH.owlClassA.stringAttribute > :str";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a <" + Vocabulary.c_OwlClassG + "> . ?x <" + Vocabulary.p_g_hasH + "> ?owlClassH . ?owlClassH <" + Vocabulary.p_h_hasA + "> ?owlClassA . ?owlClassA <" + Vocabulary.p_a_stringAttribute + "> ?gOwlClassHOwlClassAStringAttribute . FILTER (?gOwlClassHOwlClassAStringAttribute > ?str) }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneOrderByQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.age > :age ORDER BY p.age DESC";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } ORDER BY DESC(?pAge) ";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneOrderByNotInWhereQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.age > :age ORDER BY p.username DESC";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?username . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } ORDER BY DESC(?username) ";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneGroupByQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.age > :age GROUP BY p.age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } GROUP BY ?pAge ";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneGroupByNotInWhereQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.age > :age GROUP BY p.gender";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x <" + Vocabulary.p_p_gender + "> ?gender . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } GROUP BY ?gender ";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByOneNotQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(2, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?username . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndNotQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?username . FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndNotQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber OR p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x <" + Vocabulary.p_p_hasPhone + "> ?phone . ?phone <" + Vocabulary.p_p_phoneNumber + "> ?phoneNumber . } UNION { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } } UNION { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrNotQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.username = :username OR NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrNotQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrOrderByNotInWhereQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.age > :age OR p.gender = :gender ORDER BY p.username DESC";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?username . { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } UNION { ?x <" + Vocabulary.p_p_gender + "> ?gender . } } ORDER BY DESC(?username) ";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrGroupByNotInWhereQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.age > :age OR p.gender = :gender GROUP BY p.username DESC";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?username . { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } UNION { ?x <" + Vocabulary.p_p_gender + "> ?gender . } } GROUP BY ?username ";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndOrQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber AND p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x <" + Vocabulary.p_p_hasPhone + "> ?phone . ?phone <" + Vocabulary.p_p_phoneNumber + "> ?phoneNumber . ?x <" + Vocabulary.p_p_gender + "> ?gender . } UNION { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(6, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndOrQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE NOT p.phone.number = :phoneNumber AND p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x <" + Vocabulary.p_p_gender + "> ?gender . FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_hasPhone + "> ?phone . ?phone <" + Vocabulary.p_p_phoneNumber + "> ?phoneNumber . } } UNION { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(6, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndNotOrQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber AND NOT p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x <" + Vocabulary.p_p_hasPhone + "> ?phone . ?phone <" + Vocabulary.p_p_phoneNumber + "> ?phoneNumber . FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_gender + "> ?gender . } } UNION { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(6, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndOrNotQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber AND p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x <" + Vocabulary.p_p_hasPhone + "> ?phone . ?phone <" + Vocabulary.p_p_phoneNumber + "> ?phoneNumber . ?x <" + Vocabulary.p_p_gender + "> ?gender . } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(6, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndNotOrQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . ?x <" + Vocabulary.p_p_gender + "> ?gender . } } UNION { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndNotOrNotQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.username = :username AND NOT p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x " + strUri(Vocabulary.p_p_username) + " ?username . FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_gender + "> ?gender . } } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndOrNotQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE NOT p.username = :username AND p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x <" + Vocabulary.p_p_gender + "> ?gender . FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndAndQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.username = :username AND p.gender = :gender AND p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?username . ?x <" + Vocabulary.p_p_gender + "> ?gender . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndAndQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE NOT p.username = :username AND p.gender = :gender AND p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x <" + Vocabulary.p_p_gender + "> ?gender . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndNotAndQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.username = :username AND NOT p.gender = :gender AND p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?username . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_gender + "> ?gender . } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndAndNotQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.username = :username AND p.gender = :gender AND NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?username . ?x <" + Vocabulary.p_p_gender + "> ?gender . FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndNotAndQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.gender = :gender AND p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_age) + " ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . ?x <" + Vocabulary.p_p_gender + "> ?gender . } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndNotAndNotQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.username = :username AND NOT p.gender = :gender AND NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?username . FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_gender + "> ?gender . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndAndNotQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE NOT p.username = :username AND p.gender = :gender AND NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x <" + Vocabulary.p_p_gender + "> ?gender . FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndNotAndNotQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.gender = :gender AND NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . ?x <" + Vocabulary.p_p_gender + "> ?gender . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrOrQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.username = :username OR p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } UNION { ?x <" + Vocabulary.p_p_gender + "> ?gender . } UNION { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrOrQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE NOT p.username = :username OR p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } } UNION { ?x <" + Vocabulary.p_p_gender + "> ?gender . } UNION { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrNotOrQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.username = :username OR NOT p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_gender + "> ?gender . } } UNION { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrOrNotQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.username = :username OR p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } UNION { ?x <" + Vocabulary.p_p_gender + "> ?gender . } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrNotOrQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE NOT p.username = :username OR NOT p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_gender + "> ?gender . } } UNION { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrNotOrNotQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE p.username = :username OR NOT p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_gender + "> ?gender . } } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrOrNotQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE NOT p.username = :username OR p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } } UNION { ?x <" + Vocabulary.p_p_gender + "> ?gender . } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrNotOrNotQuery() {
        final String soqlQuery =
                "SELECT p FROM Person p WHERE NOT p.username = :username OR NOT p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . { FILTER NOT EXISTS { ?x " + strUri(Vocabulary.p_p_username) + " ?username . } } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_gender + "> ?gender . } } UNION { FILTER NOT EXISTS { ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    void testParseFindByAttributeValueInVariable() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.username IN :authorizedUsers";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?pUsername . FILTER (?pUsername IN (?authorizedUsers)) }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertNotNull(holder.getParameter("authorizedUsers"));
    }

    @Test
    void testParseFindByAttributeValueNotInVariable() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.username NOT IN :authorizedUsers";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?pUsername . FILTER (?pUsername NOT IN (?authorizedUsers)) }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertNotNull(holder.getParameter("authorizedUsers"));
    }

    @Test
    void testParseFindByAttributeValueInVariableWrappedInParentheses() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.username IN (:authorizedUsers)";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?pUsername . FILTER (?pUsername IN (?authorizedUsers)) }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertNotNull(holder.getParameter("authorizedUsers"));
    }

    @Test
    public void testParseFindOneNotLikeQuery() {
        final String soqlQuery = "SELECT p FROM Person p WHERE p.username NOT LIKE :username";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?pUsername . FILTER (!REGEX(?pUsername, ?username)) }";
        final QueryHolder holder = sut.parseQuery(soqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
        assertNotNull(holder.getParameter("username"));
    }

    @Test
    public void testParseInequalityOperatorQuery() {
        final String standardSoql = "SELECT p FROM Person p WHERE p.username <> :username";
        final String javaLikeSoql = "SELECT p FROM Person p WHERE p.username != :username";
        final String expectedSparqlQuery =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x " + strUri(Vocabulary.p_p_username) + " ?pUsername . FILTER (?pUsername != ?username) }";
        parseAndAssertEquality(standardSoql, expectedSparqlQuery);
        parseAndAssertEquality(javaLikeSoql, expectedSparqlQuery);
    }

    private void parseAndAssertEquality(String soql, String expectedSparql) {
        final QueryHolder holder = sut.parseQuery(soql);
        assertEquals(expectedSparql.trim(), holder.getQuery().trim());
    }

    @Test
    void testParseQueryWithIdentifierVariableInEqualityExpression() {
        final String soql = "SELECT p FROM Person p WHERE p.uri = :pUri";
        final String expectedSparql =
                "SELECT ?pUri WHERE { ?pUri a " + strUri(Vocabulary.c_Person) + " . }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void testParseQueryWithIdentifierVariableInInExpression() {
        final String soql = "SELECT p FROM Person p WHERE p.uri IN :uris";
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . FILTER (?x IN (?uris)) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void testParseQueryWithIdentifierVariableInInExpressionAccessedViaAttributeChain() {
        final String soql = "SELECT p FROM Person p WHERE p.phone.uri IN :uris";
        final String expectedSparql =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . " +
                        "?x <" + Vocabulary.p_p_hasPhone + "> ?phone . FILTER (?phone IN (?uris)) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void testParseQueryWithIdentifierVariableAccessViaAttributeChain() {
        final String soql = "SELECT p FROM Person p WHERE p.phone.uri = :phoneUri";
        final String expectedSparql =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . " +
                        "?x <" + Vocabulary.p_p_hasPhone + "> ?phoneUri . }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @ParameterizedTest
    @CsvSource({
            "UPPER, UCASE",
            "LOWER, LCASE"
    })
    void testParseQueryWithStringCapitalizationFunctionsGeneratesFilterWithSparqlFunctions(String soqlFunction,
                                                                                           String sparqlFunction) {
        final String soql = "SELECT p FROM Person p WHERE " + soqlFunction + "(p.username) = :username";
        final String expectedSparql = "SELECT ?x WHERE { " +
                "?x a " + strUri(Vocabulary.c_Person) + " . " +
                "?x " + strUri(Vocabulary.p_p_username) + " ?pUsername . " +
                "FILTER (" + sparqlFunction + "(?pUsername) = ?username) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void testParseQueryWithUpperAndLikeOperators() {
        final String soql = "SELECT p FROM Person p WHERE UPPER(p.username) LIKE :value";
        final String expectedSparql = "SELECT ?x WHERE { " +
                "?x a " + strUri(Vocabulary.c_Person) + " . " +
                "?x " + strUri(Vocabulary.p_p_username) + " ?pUsername . " +
                "FILTER (REGEX(UCASE(?pUsername), ?value)) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void testParseQueryWithMultipleInequalityOperatorsOnSingleAttribute() {
        final String soql = "SELECT p FROM Person p WHERE p.age >= :minAge AND p.age < :maxAge";
        final String expectedSparql = "SELECT ?x WHERE { " +
                "?x a " + strUri(Vocabulary.c_Person) + " . " +
                "?x <" + Vocabulary.p_p_age + "> ?pAge . " +
                "FILTER (?pAge >= ?minAge && ?pAge < ?maxAge) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void testParseQueryWithLength() {
        final String soql = "SELECT p FROM Person p WHERE LENGTH(p.username) <= :minLength";
        final String expectedSparql = "SELECT ?x WHERE { " +
                "?x a " + strUri(Vocabulary.c_Person) + " . " +
                "?x " + strUri(Vocabulary.p_p_username) + " ?pUsername . " +
                "FILTER (STRLEN(?pUsername) <= ?minLength) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @ParameterizedTest
    @CsvSource({
            SoqlConstants.Functions.ABS + ", ABS",
            SoqlConstants.Functions.CEIL + ", CEIL",
            SoqlConstants.Functions.FLOOR + ", FLOOR",
    })
    void testParseQueryWithNumericFunction(String soqlFunction, String sparqlFunction) {
        final String soql = "SELECT m FROM OWLClassM m WHERE " + soqlFunction + "(m.doubleAttribute) = :value";
        final String expectedSparql = "SELECT ?x WHERE { " +
                "?x a " + strUri(Vocabulary.c_OwlClassM) + " . " +
                "?x " + strUri(Vocabulary.p_m_doubleAttribute) + " ?mDoubleAttribute . " +
                "FILTER (" + sparqlFunction + "(?mDoubleAttribute) = ?value) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    private static String strUri(String uri) {
        return IdentifierTransformer.stringifyIri(uri);
    }

    @Test
    void parseQueryWithIdentifierAndAttributeIsCommutative() {
        final String soqlIdFirst = "SELECT p FROM Person p WHERE p.uri = :uri AND p.username = :username";
        final String soqlIdSecond = "SELECT p FROM Person p WHERE p.username = :username AND p.uri = :uri";
        final String expectedSparql = "SELECT ?uri WHERE { ?uri a " + strUri(Vocabulary.c_Person) + " . " +
                "?uri " + strUri(Vocabulary.p_p_username) + " ?username . }";
        parseAndAssertEquality(soqlIdFirst, expectedSparql);
        parseAndAssertEquality(soqlIdSecond, expectedSparql);
    }

    @Test
    void parseQueryWithTypesAndMemberOf() {
        final String soql = "SELECT p FROM Person p WHERE :type MEMBER OF p.types";
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x a ?type . }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQueryWithMemberOfPluralAttribute() {
        final String soql = "SELECT f FROM OWLClassF f WHERE :aInstance MEMBER OF f.simpleSet";
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_OwlClassF) + " . ?x " + strUri(Vocabulary.p_f_setAttribute) + " ?aInstance . }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQuerySupportsCountWithProjectedAttribute() {
        final String soqlIdFirst = "SELECT COUNT(d.owlClassA) FROM OWLClassD d WHERE d.uri = :uri";
        final String expectedSparql = "SELECT (COUNT(?owlClassA) AS ?count) WHERE { ?uri a " + strUri(Vocabulary.c_OwlClassD) + " . " +
                "?uri " + strUri(Vocabulary.p_h_hasA) + " ?owlClassA . }";
        parseAndAssertEquality(soqlIdFirst, expectedSparql);
    }

    @Test
    void parseQuerySupportsDistinctCountWithProjectedAttribute() {
        final String soqlIdFirst = "SELECT DISTINCT COUNT(d.owlClassA) FROM OWLClassD d WHERE d.uri = :uri";
        final String expectedSparql = "SELECT (COUNT(DISTINCT ?owlClassA) AS ?count) WHERE { ?uri a " + strUri(Vocabulary.c_OwlClassD) + " . " +
                "?uri " + strUri(Vocabulary.p_h_hasA) + " ?owlClassA . }";
        parseAndAssertEquality(soqlIdFirst, expectedSparql);
    }

    @Test
    void parseQueryWithProjectedAttribute() {
        final String soqlIdFirst = "SELECT d.owlClassA FROM OWLClassD d WHERE d.uri = :uri";
        final String expectedSparql = "SELECT ?owlClassA WHERE { ?uri a " + strUri(Vocabulary.c_OwlClassD) + " . " +
                "?uri " + strUri(Vocabulary.p_h_hasA) + " ?owlClassA . }";
        parseAndAssertEquality(soqlIdFirst, expectedSparql);
    }

    @Test
    void parseQueryWithProjectedAttributeAndRelatedAttribute() {
        final String soqlIdFirst = "SELECT d.owlClassA FROM OWLClassD d WHERE d.uri = :uri AND d.owlClassA.stringAttribute = :stringAtt";
        final String expectedSparql = "SELECT ?owlClassA WHERE { ?uri a " + strUri(Vocabulary.c_OwlClassD) + " . " +
                "?uri " + strUri(Vocabulary.p_h_hasA) + " ?owlClassA . " +
                "?owlClassA " + strUri(Vocabulary.p_a_stringAttribute) + " ?stringAtt . }";
        parseAndAssertEquality(soqlIdFirst, expectedSparql);
    }

    @Test
    void parseQueryWithDistinctProjectedAttributeAndRelatedAttribute() {
        final String soqlIdFirst = "SELECT DISTINCT d.owlClassA FROM OWLClassD d WHERE d.uri = :uri AND d.owlClassA.stringAttribute = :stringAtt";
        final String expectedSparql = "SELECT DISTINCT ?owlClassA WHERE { ?uri a " + strUri(Vocabulary.c_OwlClassD) + " . " +
                "?uri " + strUri(Vocabulary.p_h_hasA) + " ?owlClassA . " +
                "?owlClassA " + strUri(Vocabulary.p_a_stringAttribute) + " ?stringAtt . }";
        parseAndAssertEquality(soqlIdFirst, expectedSparql);
    }

    /**
     * Bug #178
     */
    @Test
    void parseQueryWithRelatedAttributeAndIdentifierIsCommutative() {
        final String soqlIdFirst = "SELECT d FROM OWLClassD d WHERE d.uri = :uri AND d.owlClassA.stringAttribute = :stringAtt";
        final String soqlIdSecond = "SELECT d FROM OWLClassD d WHERE d.owlClassA.stringAttribute = :stringAtt AND d.uri = :uri";
        final String expectedSparql = "SELECT ?uri WHERE { ?uri a " + strUri(Vocabulary.c_OwlClassD) + " . " +
                "?uri " + strUri(Vocabulary.p_h_hasA) + " ?owlClassA . " +
                "?owlClassA " + strUri(Vocabulary.p_a_stringAttribute) + " ?stringAtt . }";
        parseAndAssertEquality(soqlIdFirst, expectedSparql);
        parseAndAssertEquality(soqlIdSecond, expectedSparql);
    }

    @Test
    void parseQuerySupportsLangExtractionAndComparison() {
        final String soql = "SELECT u FROM OWLClassU u WHERE LANG(u.singularStringAtt) = :language";
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_OwlClassU) + " . " +
                "?x " + strUri(Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE) + " ?uSingularStringAtt . " +
                "FILTER (lang(?uSingularStringAtt) = ?language) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQuerySupportsCountById() {
        final String soql = "SELECT COUNT(a) FROM OWLClassA a WHERE a.uri = :id";
        final String expectedSparql = "SELECT (COUNT(?id) AS ?count) WHERE { ?id a " + strUri(Vocabulary.c_OwlClassA) + " . }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQueryThrowsSoqlExceptionWhenUnknownAttributeNameIsUsed() {
        final String soql = "SELECT p FROM Person p WHERE p.unknownAttribute = :param";
        when(metamodel.entity(Person.class).getAttribute(anyString())).thenThrow(IllegalArgumentException.class);
        final SoqlException ex = assertThrows(SoqlException.class, () -> sut.parseQuery(soql));
        assertThat(ex.getMessage(), containsString("No matching attribute"));
    }

    @Test
    void parseQueryTranslatesNotMemberOfToFilterNotExists() {
        final String soql = "SELECT p FROM Person p WHERE :disabledType NOT MEMBER OF p.types";
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . FILTER NOT EXISTS { ?x a ?disabledType . } }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    /**
     * Bug #234
     */
    @Test
    void parseQueryTranslatesQueryUsingRootIdentifierAfterReferenceIdentifier() {
        final String soql = "SELECT DISTINCT h FROM OWLClassH h WHERE h.owlClassA.uri = :aUri AND h.owlClassG.uri = :gUri AND h.uri = :hUri";
        final String expectedSparql = "SELECT DISTINCT ?hUri WHERE { ?hUri a " + strUri(Vocabulary.c_OwlClassH) + " . ?hUri " + strUri(Vocabulary.p_h_hasA) + " ?aUri . ?hUri " + strUri(Vocabulary.p_h_hasG) + " ?gUri . }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQueryDoesNotGenerateDuplicateTriplePatterns() {
        final String soql = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber AND p.phone.brand = :brand";
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . " +
                "?x " + strUri(Vocabulary.p_p_hasPhone) + " ?phone . " +
                "?phone " + strUri(Vocabulary.p_p_phoneNumber) + " ?phoneNumber . " +
                "?phone " + strUri(Vocabulary.p_p_phoneBrand) + " ?brand . }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQueryExtractsItemsFromRdfContainer() {
        final String soql = "SELECT c FROM OWLClassC c WHERE :param MEMBER OF c.rdfSeq";
        final int hashCode = Math.abs(strUri(Vocabulary.P_HAS_RDF_SEQ).hashCode());
        final String containerVariable = "?rdfContainer" + hashCode;
        final String hasElementVariable = "?hasElement" + hashCode;
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_OwlClassC) + " . " +
                "?x " + strUri(Vocabulary.P_HAS_RDF_SEQ) + " " + containerVariable + " . " +
                containerVariable + " " + hasElementVariable + " ?param . " +
                "FILTER (STRSTARTS(STR(" + hasElementVariable + "), \"" + RDF.NAMESPACE + "_\")) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQueryUsesDefaultAscendingOrdering() {
        final String soql = "SELECT p FROM Person p WHERE p.age > :age ORDER BY p.age";
        final String expectedSparql =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x <" + Vocabulary.p_p_age + "> ?pAge . FILTER (?pAge > ?age) } ORDER BY ASC(?pAge)";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQueryCorrectlyInterpretsOrdering() {
        final String soql = "SELECT p FROM Person p ORDER BY p.age ASC";
        final String expectedSparql =
                "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . ?x <" + Vocabulary.p_p_age + "> ?age . } ORDER BY ASC(?age)";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQueryExtractsItemsFromRdfList() {
        final String soql = "SELECT c FROM OWLClassC c WHERE :param MEMBER OF c.rdfCollection";
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_OwlClassC) + " . " +
                "?x " + strUri(Vocabulary.P_HAS_RDF_COLLECTION) + "/(" + strUri(RDF.REST) + "*/" + strUri(RDF.FIRST) + ")* ?param . " +
                "FILTER (!isBlank(?param)) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQueryExtractsItemsFromOwlSimpleList() {
        final String soql = "SELECT c FROM OWLClassC c WHERE :param MEMBER OF c.simpleList";
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_OwlClassC) + " . " +
                "?x " + strUri(Vocabulary.P_HAS_SIMPLE_LIST) + "/" + strUri(SequencesVocabulary.s_p_hasNext) + "* ?param . " +
                "}";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQueryExtractsItemsFromOwlReferencedList() {
        final String soql = "SELECT c FROM OWLClassC c WHERE :param MEMBER OF c.referencedList";
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_OwlClassC) + " . " +
                "?x " + strUri(Vocabulary.P_HAS_REFERENCED_LIST) +
                "/(" + strUri(SequencesVocabulary.s_p_hasNext) + "*/" + strUri(SequencesVocabulary.s_p_hasContents) + ") ?param . " +
                "FILTER (!isBlank(?param)) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQueryExtractsMultipleItemsFromRdfContainer() {
        final String soql = "SELECT owlclassc FROM OWLClassC owlclassc WHERE :generatedName0 MEMBER OF owlclassc.rdfSeq AND :generatedName1 MEMBER OF owlclassc.rdfSeq";
        final String result = sut.parseQuery(soql).getQuery();
        assertThat(result, matchesPattern("(.*FILTER.*){2}"));
    }

    @Test
    void parseQuerySupportsMultiParameterFunctions() {
        final String soql = "SELECT p FROM Person p WHERE CONCAT(p.firstName, p.lastName) = :name";
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_Person) + " . " +
                "?x " + strUri(Vocabulary.p_p_firstName) + " ?pFirstName . " +
                "?x " + strUri(Vocabulary.p_p_lastName) + " ?pLastName . " +
                "FILTER (CONCAT(?pFirstName, ?pLastName) = ?name) }";
        parseAndAssertEquality(soql, expectedSparql);
    }

    @Test
    void parseQuerySupportsLangMatchesFunction() {
        final String soql = "SELECT u FROM OWLClassU u WHERE LANGMATCHES(LANG(u.singularStringAtt), :language)";
        final String expectedSparql = "SELECT ?x WHERE { ?x a " + strUri(Vocabulary.c_OwlClassU) + " . " +
                "?x " + strUri(Vocabulary.P_U_SINGULAR_MULTILINGUAL_ATTRIBUTE) + " ?uSingularStringAtt . " +
                "FILTER (langMatches(lang(?uSingularStringAtt), ?language)) }";
        parseAndAssertEquality(soql, expectedSparql);
    }
}
