/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.exception.QueryParserException;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.QueryParser;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class SparqlQueryParserTest {

    private QueryParser queryParser = new SparqlQueryParser();

    @Test
    public void testParseSimpleQueryWithoutParameters() throws Exception {
        final String query = "SELECT * WHERE { <http://a> <http://b> <http://c> . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertTrue(holder.getParameters().isEmpty());
        assertEquals(query, holder.assembleQuery());
        assertEquals(query, holder.getQuery());
    }

    @Test
    public void testParseQueryWithSingleParameter() throws Exception {
        final String query = "SELECT ?x WHERE { ?x <http://www.w3.org/2000/01/rdf-schema#label> \"Test\" . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(1, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x")));
    }

    @Test
    public void testParseQueryWithThreeParametersInSingleBGP() throws Exception {
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(3, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x")));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("y")));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("z")));
    }

    @Test
    public void testParseQueryWithMultiplePatternsAndParameters() throws Exception {
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name \"Apollo 7\" .\n" +
                "?craft foaf:homepage ?homepage\n" +
                "}";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("craft")));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("homepage")));
    }

    @Test
    public void testParseQueryWithFilterAndRegex() throws Exception {
        final String query = "PREFIX  dc:  <http://purl.org/dc/elements/1.1/>\n" +
                "SELECT  ?title\n" +
                "WHERE   { ?x dc:title ?title\n" +
                "          FILTER regex(?title, \"^SPARQL\") \n" +
                "        }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("title")));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x")));
    }

    @Test
    public void testParseQueryWithFilterAndMultipleParameters() throws Exception {
        final String query = "PREFIX  dc:  <http://purl.org/dc/elements/1.1/>\n" +
                "PREFIX  ns:  <http://example.org/ns#>\n" +
                "SELECT  ?title ?price\n" +
                "WHERE   { ?x ns:price ?price .\n" +
                "          FILTER (?price < 30.5)\n" +
                "          ?x dc:title ?title . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(3, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("title")));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("price")));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x")));
    }

    @Test
    public void testParseConstructQuery() throws Exception {
        final String query = "PREFIX foaf:   <http://xmlns.com/foaf/0.1/>\n" +
                "PREFIX org:    <http://example.com/ns#>\n" +
                "\n" +
                "CONSTRUCT { ?x foaf:name ?name }\n" +
                "WHERE  { ?x org:employeeName ?name }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x")));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("name")));
    }

    @Test(expected = QueryParserException.class)
    public void parserThrowsExceptionWhenParameterNameIsMissing() throws Exception {
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ? . }";
        queryParser.parseQuery(query);
    }

    @Test
    public void testParseQueryWithNumberedPositionalParams() throws Exception {
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name $1 .\n" +
                "?craft foaf:homepage $2\n" +
                "}";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(3, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("craft")));
        assertTrue(holder.getParameters().contains(new QueryParameter<>(1)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>(2)));
    }

    @Test
    public void testParseQueryWithUnnumberedPositionalParams() throws Exception {
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name $ .\n" +
                "?craft foaf:homepage $\n" +
                "}";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(3, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("craft")));
        assertTrue(holder.getParameters().contains(new QueryParameter<>(1)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>(2)));
    }

    @Test
    public void testParseQueryWithMixedNumberedAndUnnumberedPositionalParams() throws Exception {
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name $1 .\n" +
                "?craft foaf:homepage $\n" +
                "}";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(3, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("craft")));
        assertTrue(holder.getParameters().contains(new QueryParameter<>(1)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>(2)));
    }

    @Test(expected = QueryParserException.class)
    public void parsingQueryWithUsedParameterPositionThrowsException() throws Exception {
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name $1 .\n" +
                "?craft foaf:homepage $1\n" +
                "}";
        queryParser.parseQuery(query);
    }

    @Test(expected = QueryParserException.class)
    public void parsingQueryWithIncorrectlyMixedNumberedAndUnnumberedPositionsThrowsException() throws Exception {
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name $1 .\n" +
                "?craft foaf:homepage $\n" +    // Missing number here
                "?craft rdf:label $2\n" +
                // And here we're trying to use 2, but it will have been assigned to the previous one
                "}";
        queryParser.parseQuery(query);
    }

    @Test(expected = QueryParserException.class)
    public void parsingQueryWithPositionalParameterNotNumberThrowsException() throws Exception {
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name $1 .\n" +
                "?craft foaf:homepage $notanumber\n" +
                "}";
        queryParser.parseQuery(query);
    }

    @Test
    public void parsesQueryWithOrderByDescendingAtEnd() throws Exception {
        final String query = "SELECT ?x WHERE { ?x a <http://a>. } ORDER BY DESC(?x)";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(1, holder.getParameters().size());
        assertEquals(query, holder.assembleQuery());
    }

    @Test
    public void parsesQueryWrittenInCondensedString() throws Exception {
        final String query = "SELECT ?x WHERE { ?x a ?y. } ORDER BY DESC(?y)";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x")));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("y")));
        assertEquals(query, holder.assembleQuery());
    }

    @Test
    public void parsesQueryWithOrderByAtEnd() throws Exception {
        final String query = "SELECT ?x WHERE { ?x a <http://a>. } ORDER BY ?x";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(1, holder.getParameters().size());
        assertEquals(query, holder.assembleQuery());
    }
}