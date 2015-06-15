package cz.cvut.kbss.jopa.query.impl;

import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParser;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * @author kidney
 */
public class SparqlQueryParserTest {

    private QueryParser queryParser = new SparqlQueryParser();

    @Test
    public void testParseSimpleQueryWithoutParameters() throws Exception {
        final String query = "SELECT * WHERE { <http://a> <http://b> <http://c> . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertTrue(holder.getParameters().isEmpty());
        assertEquals(query, holder.assembleQuery());
    }

    @Test
    public void testParseQueryWithSingleParameter() throws Exception {
        final String query = "SELECT ?x WHERE { ?x <http://www.w3.org/2000/01/rdf-schema#label> \"Test\" . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(1, holder.getParameters().size());
        assertTrue(holder.getParameters().contains("x"));
    }

    @Test
    public void testParseQueryWithThreeParametersInSingleBGP() throws Exception {
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(3, holder.getParameters().size());
        assertTrue(holder.getParameters().contains("x"));
        assertTrue(holder.getParameters().contains("y"));
        assertTrue(holder.getParameters().contains("z"));
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
        assertTrue(holder.getParameters().contains("craft"));
        assertTrue(holder.getParameters().contains("homepage"));
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
        assertTrue(holder.getParameters().contains("title"));
        assertTrue(holder.getParameters().contains("x"));
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
        assertTrue(holder.getParameters().contains("title"));
        assertTrue(holder.getParameters().contains("price"));
        assertTrue(holder.getParameters().contains("x"));
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
        assertTrue(holder.getParameters().contains("x"));
        assertTrue(holder.getParameters().contains("name"));
    }
}