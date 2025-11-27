package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.query.sparql.loader.SparqlAssemblyModifier;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class SparqlQueryParsingAndAssemblyTest {

    private static final String SIMPLE_QUERY = "SELECT ?x WHERE { ?x a ?type . }";
    private static final String ORDINAL_QUERY = "SELECT ?x WHERE { ?x a $1 . }";

    private ParameterValueFactory valueFactory;

    private final Configuration configuration = new Configuration();

    private Sparql11QueryParser queryParser;

    private TokenStreamSparqlQueryHolder sut;

    @BeforeEach
    void setUp() {
        UnitOfWork uowMock = mock(UnitOfWork.class);
        when(uowMock.getConfiguration()).thenReturn(configuration);
        this.valueFactory = new ParameterValueFactory(uowMock);
        this.queryParser = new Sparql11QueryParser(valueFactory);
    }

    @Test
    void setMaxResultsAddsLimitClauseToAssembledQuery() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        sut.setMaxResults(10);
        final String result = sut.assembleQuery();
        assertThat(result, endsWith("LIMIT 10"));
    }

    @Test
    void assembleDoesNotAddLimitClauseWhenMaxResultsWasNotSet() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        final String result = sut.assembleQuery();
        assertThat(result, not(containsString("LIMIT")));
    }

    @Test
    void setFirstResultAddsOffsetClauseToAssembledQuery() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        sut.setFirstResult(5);
        final String result = sut.assembleQuery();
        assertThat(result, endsWith("OFFSET 5"));
    }

    @Test
    void assembleDoesNotAddOffsetClauseWhenFirstResultWasNotSet() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        final String result = sut.assembleQuery();
        assertThat(result, not(containsString("OFFSET")));
    }

    @Test
    void setFirstResultAndSetMaxResultsAddsLimitAndOffsetToAssembledQuery() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        sut.setFirstResult(5);
        sut.setMaxResults(10);
        final String result = sut.assembleQuery();
        assertThat(result, containsString("LIMIT 10"));
        assertThat(result, containsString("OFFSET 5"));
    }

    @Test
    void assembleQuerySupportsPluralValues() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        final List<URI> paramValues = List.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier());
        sut.setParameter(sut.getParameter("x"), paramValues);
        final String result = sut.assembleQuery();
        assertThat(result, containsString("VALUES (?x)"));
        paramValues.forEach(v -> assertThat(result, containsString(" ( " + IdentifierTransformer.stringifyIri(v) + " )")));
    }

    @Test
    void assembleQuerySupportsValuesWithInEqualSize() {
        this.sut = queryParser.parseQuery("SELECT ?x ?type WHERE { ?x a ?type . }");
        final List<URI> xValues = List.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier());
        final List<URI> typeValues = List.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier());
        sut.setParameter(sut.getParameter("x"), xValues);
        sut.setParameter(sut.getParameter("type"), typeValues);

        final String result = sut.assembleQuery();
        assertThat(result, containsString("VALUES (?x ?type)"));
        assertThat(result, containsString("( " + IdentifierTransformer.stringifyIri(xValues.get(0)) + " " + IdentifierTransformer.stringifyIri(typeValues.get(0)) + " )"));
        assertThat(result, containsString("( " + IdentifierTransformer.stringifyIri(xValues.get(1)) + " " + IdentifierTransformer.stringifyIri(typeValues.get(1)) + " )"));
        typeValues.subList(2, typeValues.size())
                  .forEach(v -> assertThat(result, containsString(" ( UNDEF " + IdentifierTransformer.stringifyIri(v) + " )")));
    }

    @Test
    void getParametersReturnsAllParametersFromQuery() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        final Set<Parameter<?>> parameters = sut.getParameters();
        assertEquals(2, parameters.size());
        assertThat(parameters.stream().map(Parameter::getName).collect(Collectors.toSet()), hasItems("x", "type"));
    }

    @Test
    void getParameterByNameReturnsCorrectParameter() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        final Parameter<?> xParam = sut.getParameter("x");
        assertEquals("x", xParam.getName());
    }

    @Test
    void getParameterByUnknownNameThrowsIllegalArgumentException() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        assertThrows(IllegalArgumentException.class, () -> sut.getParameter("unknown"));
    }

    @Test
    void setParameterSetsParameterValue() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        final String value = Generators.createIndividualIdentifier().toString();
        sut.setParameter(new QueryParameter<>("type", valueFactory), value);
        assertEquals(value, sut.getParameterValue(sut.getParameter("type")));
    }

    @Test
    void setParameterThrowsNullPointerExceptionForNullValue() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        assertThrows(NullPointerException.class, () -> sut.setParameter(new QueryParameter<>("type", valueFactory), null));
    }

    @Test
    void setParameterThrowsIllegalArgumentExceptionForUnknownParameter() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        assertThrows(IllegalArgumentException.class, () -> sut.setParameter(new QueryParameter<>("unknown", valueFactory), "value"));
    }

    @Test
    void clearParameterRemovesParameterValue() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        final QueryParameter<?> qp = new QueryParameter<>("type", valueFactory);
        sut.setParameter(qp, Generators.createIndividualIdentifier());
        assertNotNull(sut.getParameterValue(qp));
        sut.clearParameter(qp);
        assertNull(sut.getParameterValue(qp));
    }

    @Test
    void clearParametersRemovesAllParameterValues() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        final QueryParameter<?> qp = new QueryParameter<>("x", valueFactory);
        sut.setParameter(qp, URI.create("https://kbss.felk.cvut.cz"));
        final QueryParameter<?> qpTwo = new QueryParameter<>("type", valueFactory);
        sut.setParameter(qpTwo, Generators.createIndividualIdentifier());
        sut.getParameters().forEach(param -> assertNotNull(sut.getParameterValue(param)));
        sut.clearParameters();
        sut.getParameters().forEach(param -> assertNull(sut.getParameterValue(param)));
    }

    @Test
    void testGetOrdinalParameter() {
        this.sut = queryParser.parseQuery(ORDINAL_QUERY);
        final int position = 1;
        final Parameter<?> param = sut.getParameter(position);
        assertEquals(position, param.getPosition().intValue());
    }

    @Test
    void getParameterWithUnknownIndexThrowsException() {
        this.sut = queryParser.parseQuery(ORDINAL_QUERY);
        assertThrows(IllegalArgumentException.class, () -> sut.getParameter(Integer.MAX_VALUE));
    }

    @Test
    void testSetOrdinalParameterValue() {
        this.sut = queryParser.parseQuery(ORDINAL_QUERY);
        final String value = Generators.createIndividualIdentifier().toString();
        final QueryParameter<?> param = new QueryParameter<>(1, valueFactory);
        sut.setParameter(param, value);
        assertEquals(value, sut.getParameterValue(param));
    }

    @Test
    void setValueOfUnknownParameterThrowsException() {
        this.sut = queryParser.parseQuery(ORDINAL_QUERY);
        assertThrows(IllegalArgumentException.class, () -> sut
                .setParameter(new QueryParameter<>(Integer.MAX_VALUE, valueFactory), "value"));
    }

    @Test
    void assembleQuerySetsUriNamedParameterValueToQueryVariable() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        final URI value = Generators.createIndividualIdentifier();
        sut.setParameter(new QueryParameter<>("type", valueFactory), value);
        final String result = sut.assembleQuery();
        assertEquals(SIMPLE_QUERY.replace("?type", "<" + value + ">"), result);
    }

    @Test
    void assembleQuerySetsUriOrdinalParameterValueToQueryVariable() {
        this.sut = queryParser.parseQuery(ORDINAL_QUERY);
        final URI value = Generators.createIndividualIdentifier();
        sut.setParameter(new QueryParameter<>(1, valueFactory), value);
        final String result = sut.assembleQuery();
        assertEquals(ORDINAL_QUERY.replace("$1", "<" + value + ">"), result);
    }

    @Test
    void assembleQuerySetsOnlySpecifiedOrdinalParameters() {
        this.sut = queryParser.parseQuery("SELECT ?x WHERE { ?x a $1 ; rdfs:label $2 }");
        final URI value = Generators.createIndividualIdentifier();
        sut.setParameter(sut.getParameter(1), value);
        assertEquals("SELECT ?x WHERE { ?x a $1 ; rdfs:label $2 }".replace("$1", "<" + value + ">"), sut.assembleQuery());
    }

    @Test
    void assembleQuerySetsStringLiteralParameterValueToQueryVariable() {
        this.sut = queryParser.parseQuery("SELECT ?x WHERE { ?x rdfs:label ?label }");
        sut.setParameter(sut.getParameter("label"), "NASA");
        assertEquals("SELECT ?x WHERE { ?x rdfs:label \"NASA\" }", sut.assembleQuery());
    }

    @Test
    void assembleQuerySetsTypedLiteralParameterValueToQueryVariable() {
        this.sut = queryParser.parseQuery("SELECT ?p WHERE { ?p foaf:age ?age }");
        sut.setParameter(sut.getParameter("age"), 18);
        assertEquals("SELECT ?p WHERE { ?p foaf:age \"18\"^^<" + XSD.INT + "> }", sut.assembleQuery());
    }

    @Test
    void assembleQuerySetsParameterValueToQueryVariableWhenMultipleVariablesAreNextToEachOther() {
        this.sut = queryParser.parseQuery("SELECT ?y ?z WHERE { ?x ?y ?z . }");
        final URI value = Generators.createIndividualIdentifier();
        sut.setParameter(sut.getParameter("x"), value);
        assertEquals("SELECT ?y ?z WHERE { <" + value + "> ?y ?z . }", sut.assembleQuery());
    }

    @Test
    void assembleQueryAddsValuesClauseWhenSetParameterIsInProjection() {
        this.sut = queryParser.parseQuery("SELECT ?x ?type WHERE { ?x a ?type . }");
        final URI value = Generators.createIndividualIdentifier();
        sut.setParameter(sut.getParameter("x"), value);
        final String result = sut.assembleQuery();
        assertThat(result, Matchers.endsWith("VALUES (?x) { ( <" + value + "> ) }"));
        assertThat(result, Matchers.containsString("SELECT ?x ?type WHERE { ?x a ?type . }"));
    }

    @Test
    void setFirstResultThrowsIllegalStateExceptionWhenQueryContainsOffsetClause() {
        this.sut = queryParser.parseQuery("SELECT ?x ?type WHERE { ?x a ?type . } OFFSET 1");
        assertThrows(IllegalStateException.class, () -> sut.setFirstResult(2));
    }

    @Test
    void setMaxResultsThrowsIllegalStateExceptionWhenQueryContainsLimitClause() {
        this.sut = queryParser.parseQuery("SELECT ?x ?type WHERE { ?x a ?type . } LIMIT 1");
        assertThrows(IllegalStateException.class, () -> sut.setMaxResults(2));
    }

    @Test
    void parseAndAssembleQueryAllowsModifierToInsertClausesBeforeLastClosingCurlyBrace() {
        this.sut = queryParser.parseQuery(SIMPLE_QUERY);
        final SparqlAssemblyModifier
                assemblyModifier = (sut, tokenRewriter, queryAttributes) -> tokenRewriter.insertBefore(queryAttributes.lastClosingCurlyBraceToken(), "?x ?y ?z . ");
        sut.setAssemblyModifier(assemblyModifier);

        final String result = sut.assembleQuery();
        assertEquals("SELECT ?x WHERE { ?x a ?type . ?x ?y ?z . }", result);
    }

    @Test
    void parseAndAssembleAllowsVariableInDropGraphStatement() {
        this.sut = queryParser.parseQuery("DROP GRAPH ?g");
        final URI graphUri = Generators.createIndividualIdentifier();
        sut.setParameter(sut.getParameter("g"), graphUri);

        final String result = sut.assembleQuery();
        assertEquals("DROP GRAPH " + IdentifierTransformer.stringifyIri(graphUri), result);
    }

    @Test
    void parseAndAssembleSupportsVariablesInTripleTerms() {
        final URI xUri = Generators.createIndividualIdentifier();
        this.sut = queryParser.parseQuery("SELECT ?annotationProperty ?annotationValue WHERE { << ?x ?property ?value >> ?annotationProperty ?annotationValue . }");
        sut.setParameter(sut.getParameter("x"), xUri);

        final String result = sut.assembleQuery();
        assertEquals("SELECT ?annotationProperty ?annotationValue WHERE { << " + IdentifierTransformer.stringifyIri(xUri) + " ?property ?value >> ?annotationProperty ?annotationValue . }", result);
    }
}
