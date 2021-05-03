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
package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryParser;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


public class SoqlQueryParserTest {

    @Mock
    private MetamodelImpl metamodel;

    private QueryParser sut;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodel);
        final MetamodelProvider mpp = mock(MetamodelProvider.class);
        when(mpp.getMetamodel()).thenReturn(metamodel);
        when(metamodel.getEntities()).thenReturn(Collections.emptySet());
        when(mpp.isEntityType(any())).thenAnswer(inv -> metamodel.isEntityType(inv.getArgument(0)));
        final SparqlQueryParser qp = new SparqlQueryParser(new ParameterValueFactory(mpp));
        this.sut = new SoqlQueryParser(qp, metamodel);
    }

    @Test
    public void testParseFindAllQuery() {
        final String jpqlQuery = "SELECT p FROM Person p";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(1, holder.getParameters().size());
    }

    @Test
    public void testParseDistinctFindAllQuery() {
        final String jpqlQuery = "SELECT DISTINCT p FROM Person p";
        final String expectedSparqlQuery = "SELECT DISTINCT ?x WHERE { ?x a <http://www.example.org/Person> . }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(1, holder.getParameters().size());
    }

    @Test
    public void testParseCountQuery() {
        final String jpqlQuery = "SELECT COUNT(p) FROM Person p";
        final String expectedSparqlQuery = "SELECT (COUNT(?x) AS ?count) WHERE { ?x a <http://www.example.org/Person> . }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(2, holder.getParameters().size());
    }

    @Test
    public void testParseDistinctCountQuery() {
        final String jpqlQuery = "SELECT DISTINCT COUNT(p) FROM Person p";
        final String expectedSparqlQuery = "SELECT (COUNT(distinct ?x) AS ?count) WHERE { ?x a <http://www.example.org/Person> . }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(2, holder.getParameters().size());
    }

    @Test
    public void testParseFindAllOWLClassAQuery() {
        final String jpqlQuery = "SELECT a FROM OWLClassA a";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/OWLClassA> . }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(1, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(2, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneLikeQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username LIKE :username";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?pUsername . FILTER (regex(?pUsername, ?username) ) }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneJoinedQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindMutipleJoinedQuery() {
        final String jpqlQuery = "SELECT a FROM OWLClassA a WHERE a.OWLClassB.OWLClassC.OWLClassD = :d";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/OWLClassA> . ?x <http://www.example.org/OWLClassB> ?OWLClassB . ?OWLClassB <http://www.example.org/OWLClassC> ?OWLClassC . ?OWLClassC <http://www.example.org/OWLClassD> ?d . }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindMutipleJoinedQueryFilter() {
        final String jpqlQuery = "SELECT a FROM OWLClassA a WHERE a.OWLClassB.OWLClassC.OWLClassD.intAttribute > :intAttribute";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/OWLClassA> . ?x <http://www.example.org/OWLClassB> ?OWLClassB . ?OWLClassB <http://www.example.org/OWLClassC> ?OWLClassC . ?OWLClassC <http://www.example.org/OWLClassD> ?OWLClassD . ?OWLClassD <http://www.example.org/intAttribute> ?aOWLClassBOWLClassCOWLClassDIntAttribute . FILTER (?aOWLClassBOWLClassCOWLClassDIntAttribute > ?intAttribute) }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(6, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneOrderByQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age ORDER BY p.age DESC";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } ORDER BY DESC(?pAge) ";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneOrderByNotInWhereQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age ORDER BY p.username DESC";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } ORDER BY DESC(?username) ";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneGroupByQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age GROUP BY p.age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } GROUP BY ?pAge ";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneGroupByNotInWhereQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age GROUP BY p.gender";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/gender> ?gender . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } GROUP BY ?gender ";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByOneNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(2, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username OR NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrOrderByNotInWhereQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age OR p.gender = :gender ORDER BY p.username DESC";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } UNION { ?x <http://www.example.org/gender> ?gender . } } ORDER BY DESC(?username) ";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrGroupByNotInWhereQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age OR p.gender = :gender GROUP BY p.username DESC";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } UNION { ?x <http://www.example.org/gender> ?gender . } } GROUP BY ?username ";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndOrQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber AND p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . ?x <http://www.example.org/gender> ?gender . } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(6, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndOrQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.phone.number = :phoneNumber AND p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/gender> ?gender . FILTER NOT EXISTS { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(6, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndNotOrQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber AND NOT p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(6, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndOrNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber AND p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . ?x <http://www.example.org/gender> ?gender . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(6, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndNotOrQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/gender> ?gender . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndNotOrNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND NOT p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndOrNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/gender> ?gender . FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndAndQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND p.gender = :gender AND p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/gender> ?gender . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndAndQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND p.gender = :gender AND p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/gender> ?gender . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndNotAndQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND NOT p.gender = :gender AND p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndAndNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND p.gender = :gender AND NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/gender> ?gender . FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndNotAndQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.gender = :gender AND p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/gender> ?gender . } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndNotAndNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND NOT p.gender = :gender AND NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndAndNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND p.gender = :gender AND NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/gender> ?gender . FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotAndNotAndNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND NOT p.gender = :gender AND NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/gender> ?gender . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrOrQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username OR p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . } UNION { ?x <http://www.example.org/gender> ?gender . } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrOrQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { ?x <http://www.example.org/gender> ?gender . } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrNotOrQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username OR NOT p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrOrNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username OR p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . } UNION { ?x <http://www.example.org/gender> ?gender . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrNotOrQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR NOT p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrNotOrNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username OR NOT p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrOrNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { ?x <http://www.example.org/gender> ?gender . } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleNotOrNotOrNotQuery() {
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR NOT p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS { ?x <http://www.example.org/username> ?username . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/gender> ?gender . } } UNION { FILTER NOT EXISTS { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } } }";
        final QueryHolder holder = sut.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }
}
