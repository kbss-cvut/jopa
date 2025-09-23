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
package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.exception.QueryParserException;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

public class SparqlQueryParserTest {

    private QueryParser queryParser;

    private ParameterValueFactory valueFactory;

    @BeforeEach
    void setUp() {
        this.valueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));
        this.queryParser = new Sparql11QueryParser(valueFactory);
    }

    @Test
    public void testParseSimpleQueryWithoutParameters() {
        final String query = "SELECT * WHERE { <http://a> <http://b> <http://c> . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertTrue(holder.getParameters().isEmpty());
        assertEquals(query, holder.assembleQuery());
        assertEquals(query, holder.getQuery());
    }

    @Test
    public void testParseQueryWithSingleParameter() {
        final String query = "SELECT ?x WHERE { ?x <http://www.w3.org/2000/01/rdf-schema#label> \"Test\" . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(1, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x", valueFactory)));
    }

    @Test
    public void testParseQueryWithThreeParametersInSingleBGP() {
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(3, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x", valueFactory)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("y", valueFactory)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("z", valueFactory)));
    }

    @Test
    public void testParseQueryWithMultiplePatternsAndParameters() {
        final String query = """
                PREFIX foaf: <http://xmlns.com/foaf/0.1/>
                SELECT ?craft {
                  ?craft foaf:name "Apollo 7" .
                  ?craft foaf:homepage ?homepage
                }""";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("craft", valueFactory)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("homepage", valueFactory)));
    }

    @Test
    public void testParseQueryWithFilterAndRegex() {
        final String query = """
                PREFIX  dc:  <http://purl.org/dc/elements/1.1/>
                SELECT  ?title
                WHERE   { ?x dc:title ?title
                          FILTER regex(?title, "^SPARQL")\s
                        }""";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("title", valueFactory)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x", valueFactory)));
    }

    @Test
    public void testParseQueryWithFilterAndMultipleParameters() {
        final String query = """
                PREFIX  dc:  <http://purl.org/dc/elements/1.1/>
                PREFIX  ns:  <http://example.org/ns#>
                SELECT  ?title ?price
                WHERE   { ?x ns:price ?price .
                          FILTER (?price < 30.5)
                          ?x dc:title ?title . }""";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(3, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("title", valueFactory)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("price", valueFactory)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x", valueFactory)));
    }

    @Test
    public void testParseConstructQuery() {
        final String query = """
                PREFIX foaf:   <http://xmlns.com/foaf/0.1/>
                PREFIX org:    <http://example.com/ns#>
                
                CONSTRUCT { ?x foaf:name ?name }
                WHERE  { ?x org:employeeName ?name }""";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x", valueFactory)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("name", valueFactory)));
    }

    @Test
    public void parserThrowsExceptionWhenParameterNameIsMissing() {
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ? . }";
        assertThrows(QueryParserException.class, () -> queryParser.parseQuery(query));
    }

    @Test
    public void testParseQueryWithNumberedPositionalParams() {
        final String query = """
                PREFIX foaf: <http://xmlns.com/foaf/0.1/>
                SELECT ?craft
                {
                  ?craft foaf:name $1 .
                  ?craft foaf:homepage $2
                }""";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(3, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("craft", valueFactory)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>(1, valueFactory)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>(2, valueFactory)));
    }

    @Test
    public void parseQuerySupportsMultipleOccurrencesOfNumericParameter() {
        final String query = """
                PREFIX foaf: <http://xmlns.com/foaf/0.1/>
                SELECT ?craft {
                  ?craft foaf:weblog $1 .
                  ?craft foaf:homepage $1 .
                }""";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        assertTrue(holder.hasParameter("craft"));
        assertTrue(holder.hasParameter(1));
    }

    @Test
    public void parsingQueryWithIncorrectlyMixedNumberedAndUnnumberedPositionsThrowsException() {
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name $1 .\n" +
                "?craft foaf:homepage $\n" +    // Missing number here
                "?craft rdf:label $2\n" +
                // And here we're trying to use 2, but it will have been assigned to the previous one
                "}";
        assertThrows(QueryParserException.class, () -> queryParser.parseQuery(query));
    }

    @Test
    public void parsingQueryWithPositionalParameterNotNumberThrowsException() {
        final String query = """
                PREFIX foaf: <http://xmlns.com/foaf/0.1/>
                SELECT ?craft
                {
                  ?craft foaf:name $1 .
                  ?craft foaf:homepage $notanumber
                }""";
        assertThrows(QueryParserException.class, () -> queryParser.parseQuery(query));
    }

    @Test
    public void parsesQueryWithOrderByDescendingAtEnd() {
        final String query = "SELECT ?x WHERE { ?x a <http://a>. } ORDER BY DESC(?x)";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(1, holder.getParameters().size());
        assertEquals(query, holder.assembleQuery());
    }

    @Test
    public void parsesQueryWrittenInCondensedString() {
        final String query = "SELECT ?x WHERE { ?x a ?y. } ORDER BY DESC(?y)";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        assertTrue(holder.getParameters().contains(new QueryParameter<>("x", valueFactory)));
        assertTrue(holder.getParameters().contains(new QueryParameter<>("y", valueFactory)));
        assertEquals(query, holder.assembleQuery());
    }

    @Test
    public void parsesQueryWithOrderByAtEnd() {
        final String query = "SELECT ?x WHERE { ?x a <http://a>. } ORDER BY ?x";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(1, holder.getParameters().size());
        assertEquals(query, holder.assembleQuery());
    }

    /**
     * Bug #6.
     */
    @Test
    public void parsesQueryWithVariableWithoutTrailingSpace() {
        final String query = "SELECT ?x WHERE {?x a ?type}";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        assertNotNull(holder.getParameter("x"));
        assertNotNull(holder.getParameter("type"));
    }

    @Test
    public void parsesQueryWithPropertyPathOneOrMore() {
        final String query = "SELECT ?x WHERE {?x ?hasPart+ ?part . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertNotNull(holder.getParameter("x"));
        assertNotNull(holder.getParameter("hasPart"));
        assertNotNull(holder.getParameter("part"));
    }

    @Test
    public void parsesQueryWithPropertyPathZeroOrMore() {
        final String query = "SELECT ?x WHERE {?x ?hasPart* ?part . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertNotNull(holder.getParameter("x"));
        assertNotNull(holder.getParameter("hasPart"));
        assertNotNull(holder.getParameter("part"));
    }

    @Test
    public void parsesQueryWithPropertyPathZeroOrOne() {
        final String query = "SELECT ?x WHERE {?x ?hasPart? ?part . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertNotNull(holder.getParameter("x"));
        assertNotNull(holder.getParameter("hasPart"));
        assertNotNull(holder.getParameter("part"));
    }

    @Test
    public void parsesQueryWithPropertyPathSequence() {
        final String query = "SELECT ?x WHERE {?x ?hasPart/?hasQuestion ?part . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertNotNull(holder.getParameter("x"));
        assertNotNull(holder.getParameter("hasPart"));
        assertNotNull(holder.getParameter("hasQuestion"));
        assertNotNull(holder.getParameter("part"));
    }

    @Test
    public void parsesQueryWithPropertyPathAlternative() {
        final String query = "SELECT ?x WHERE {?x ?hasPart|?hasQuestion ?part . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertNotNull(holder.getParameter("x"));
        assertNotNull(holder.getParameter("hasPart"));
        assertNotNull(holder.getParameter("hasQuestion"));
        assertNotNull(holder.getParameter("part"));
    }

    @Test
    void parseQuerySupportsWindowsLikeLineEnds() {
        final String query = """
                SELECT * WHERE {\r
                   GRAPH ?contextUri\r
                        {  ?s ?p ?o . }\r\
                }""";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertNotNull(holder.getParameter("contextUri"));
        assertNotNull(holder.getParameter("s"));
        assertNotNull(holder.getParameter("p"));
        assertNotNull(holder.getParameter("o"));
    }

    @Test
    void parseQuerySetsProjectedMarkerOnVariablesAppearingInSelectProjection() {
        final String query = "SELECT ?x WHERE {?x ?hasPart|?hasQuestion ?part . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        final QueryParameter<?> xVar = (QueryParameter<?>) holder.getParameter("x");
        assertTrue(xVar.isProjected());
        final QueryParameter<?> partVar = (QueryParameter<?>) holder.getParameter("part");
        assertFalse(partVar.isProjected());
    }

    /**
     * Bug #165
     */
    @Test
    void parseQueryDoesNotRequireWhereKeywordInSelectQuery() {
        final String query = "SELECT ?x { ?x <http://www.w3.org/2000/01/rdf-schema#label> ?label . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        final QueryParameter<?> xVar = (QueryParameter<?>) holder.getParameter("x");
        assertTrue(xVar.isProjected());
        final QueryParameter<?> labelVar = (QueryParameter<?>) holder.getParameter("label");
        assertFalse(labelVar.isProjected());
    }

    /**
     * Bug #165
     */
    @Test
    void parseQueryDoesNotRequireWhereKeywordInAskQuery() {
        final String query = "ASK { ?x <http://www.w3.org/2000/01/rdf-schema#label> ?label . }";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        final QueryParameter<?> xVar = (QueryParameter<?>) holder.getParameter("x");
        assertFalse(xVar.isProjected());
        final QueryParameter<?> labelVar = (QueryParameter<?>) holder.getParameter("label");
        assertFalse(labelVar.isProjected());
    }

    @Test
    void parseQueryIgnoresQueryCommentsAtStringStart() {
        final String query = """
                # Selects ?y that have label "Test" - variables name is different to test ignoring variables in comments
                SELECT ?x WHERE {
                  ?x <http://www.w3.org/2000/01/rdf-schema#label> "Test" .
                }""";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(1, holder.getParameters().size());
        assertNotNull(holder.getParameter("x"));
    }

    @Test
    void parseQueryIgnoresQueryCommentsBetweenQueryLines() {
        final String query = """
                SELECT ?x WHERE {
                # Selects ?y that have label "Test" - variables name is different to test ignoring variables in comments
                  ?x <http://www.w3.org/2000/01/rdf-schema#label> "Test" .
                }""";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(1, holder.getParameters().size());
        assertNotNull(holder.getParameter("x"));
    }

    @Test
    void parseQueryIgnoresQueryCommentsStartingInQueryLine() {
        final String query = """
                SELECT ?x WHERE {
                  ?x a ?t#ype . is still a comment so ?y is ignored
                }""";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(2, holder.getParameters().size());
        assertNotNull(holder.getParameter("x"));
        assertNotNull(holder.getParameter("t"));
    }

    @Test
    void parseQueryIgnoresQueryCommentsStartingInFunction() {
        final String query = """
                SELECT ?x WHERE {
                  ?x a ?t#ype . is still a comment so ?y is ignored
                  BIND (STR(?t) as ?typeStr)# as ?y)
                }""";
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(3, holder.getParameters().size());
        assertNotNull(holder.getParameter("x"));
        assertNotNull(holder.getParameter("t"));
        assertNotNull(holder.getParameter("typeStr"));
    }

    @Test
    void parseQueryHandlesAsk() {
        final String query = """
                ASK {
                  ?c ?hasContainer ?container .
                  ?container ?hasMember ?a .
                  FILTER (STRSTARTS(STR(?hasMember), "rdf:_")) }
                """;
        final QueryHolder holder = queryParser.parseQuery(query);
        assertNotNull(holder.getParameter("c"));
        assertNotNull(holder.getParameter("hasContainer"));
        assertNotNull(holder.getParameter("container"));
        assertNotNull(holder.getParameter("hasMember"));
        assertNotNull(holder.getParameter("a"));
    }

    @Test
    void parseQueryHandlesMultilineProjection() {
        final String query = """
                SELECT ?x
                  # ?y is a property that was previously called ?p
                  ?y
                  ?z
                WHERE {
                  ?x ?y ?z .
                }
                """;
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(3, holder.getParameters().size());
        assertNotNull(holder.getParameter("x"));
        assertNotNull(holder.getParameter("y"));
        assertNotNull(holder.getParameter("z"));
    }

    /**
     * Bug #348
     */
    @Test
    void parseQueryDoesNotInterpretHashtagInIriAsComment() {
        final String query = """
                SELECT ?x WHERE {
                  ?x a <https://onto.fel.cvut.cz/ontologies/jopa/types#OWLClassC> .
                  ?x <https://onto.fel.cvut.cz/ontologies/jopa/attributes#hasRdfBag> ?rdfContainer .
                  ?rdfContainer ?hasElement ?generatedName0 .
                  FILTER (STRSTARTS(STR(?hasElement), "http://www.w3.org/1999/02/22-rdf-syntax-ns#_"))
                  ?x <https://onto.fel.cvut.cz/ontologies/jopa/attributes#hasSimpleSequence>/<http://krizik.felk.cvut.cz/ontologies/2008/sequences.owl#hasNext>* ?generatedName1 .
                }
                """;
        final QueryHolder holder = queryParser.parseQuery(query);
        assertTrue(holder.hasParameter("generatedName0"));
        assertTrue(holder.hasParameter("generatedName1"));
    }

    @ParameterizedTest
    @MethodSource("queryTypesParams")
    void parseQueryResolvesQueryType(String query, QueryType expectedType) {
        final QueryHolder holder = queryParser.parseQuery(query);
        assertEquals(expectedType, holder.getQueryType());
    }

    static Stream<Arguments> queryTypesParams() {
        return Stream.of(
                Arguments.of("SELECT ?x WHERE { ?x a ?z . }", QueryType.SELECT),
                Arguments.of("ASK { ?x a <https://example.com/a> . }", QueryType.ASK),
                Arguments.of("INSERT DATA { <https://example.com/a> rdf:type <https://example.com/b> . }", QueryType.INSERT),
                Arguments.of("DELETE {?x ?y ?z } WHERE { ?x a ?z . }", QueryType.DELETE),
                Arguments.of("CONSTRUCT { ?x a <https://example.com/b> . } WHERE { ?x a <https://example.com/a> . }", QueryType.CONSTRUCT)
        );
    }
}
