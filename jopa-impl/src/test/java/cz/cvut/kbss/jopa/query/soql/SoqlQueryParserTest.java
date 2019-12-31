/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParser;
import org.junit.Test;

import static org.junit.Assert.*;

public class SoqlQueryParserTest {

    private QueryParser queryParser = new SoqlQueryParser();

    @Test
    public void testParseFindAllQuery(){
        final String jpqlQuery = "SELECT p FROM Person p";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(1, holder.getParameters().size());
    }

    @Test
    public void testParseDistinctFindAllQuery(){
        final String jpqlQuery = "SELECT DISTINCT p FROM Person p";
        final String expectedSparqlQuery = "SELECT DISTINCT ?x WHERE { ?x a <http://www.example.org/Person> . }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(1, holder.getParameters().size());
    }

    @Test
    public void testParseCountQuery(){
        final String jpqlQuery = "SELECT DISTINCT COUNT(p) FROM Person p";
        final String expectedSparqlQuery = "SELECT (COUNT(distinct ?x) AS ?count) WHERE { ?x a <http://www.example.org/Person> . }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(2, holder.getParameters().size());
    }

    @Test
    public void testParseFindAllOWLClassAQuery(){
        final String jpqlQuery = "SELECT a FROM OWLClassA a";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/OWLClassA> . }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(1, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(2, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneLikeQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username LIKE :username";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?pUsername . FILTER (regex(?pUsername, ?username) ) }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneJoinedQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneOrderByQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age ORDER BY p.age DESC";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } ORDER BY DESC(?pAge) ";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneOrderByNotInWhereQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age ORDER BY p.username DESC";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } ORDER BY DESC(?username) ";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindOneGroupByQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age GROUP BY p.age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } GROUP BY ?pAge ";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(3, holder.getParameters().size());
    }

    @Test
    public void testParseFindByOneNotQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . FILTER NOT EXISTS ( ?x <http://www.example.org/username> ?username . ) }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(2, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrOrderByNotInWhereQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age OR p.gender = :gender ORDER BY p.username DESC";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } UNION { ?x <http://www.example.org/gender> ?gender . } } ORDER BY DESC(?username) ";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrGroupByNotInWhereQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.age > :age OR p.gender = :gender GROUP BY p.username DESC";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/username> ?username . { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } UNION { ?x <http://www.example.org/gender> ?gender . } } GROUP BY ?username ";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndOrQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.phone.number = :phoneNumber AND p.gender = :gender OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/phone> ?phone . ?phone <http://www.example.org/number> ?phoneNumber . ?x <http://www.example.org/gender> ?gender . } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(6, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndNotQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username AND p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) FILTER NOT EXISTS ( ?x <http://www.example.org/username> ?username . ) }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleOrNotQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE NOT p.username = :username OR p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { FILTER NOT EXISTS ( ?x <http://www.example.org/username> ?username . ) } UNION { ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) } }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(4, holder.getParameters().size());
    }

    @Test
    public void testParseFindByMultipleAndOrNotQuery(){
        final String jpqlQuery = "SELECT p FROM Person p WHERE p.username = :username AND NOT p.gender = :gender OR NOT p.age > :age";
        final String expectedSparqlQuery = "SELECT ?x WHERE { ?x a <http://www.example.org/Person> . { ?x <http://www.example.org/username> ?username . FILTER NOT EXISTS ( ?x <http://www.example.org/gender> ?gender . ) } UNION { FILTER NOT EXISTS ( ?x <http://www.example.org/age> ?pAge . FILTER (?pAge > ?age) ) } }";
        final QueryHolder holder = queryParser.parseQuery(jpqlQuery);
        assertEquals(expectedSparqlQuery, holder.getQuery());
        assertEquals(5, holder.getParameters().size());
    }
}