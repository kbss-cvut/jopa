package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.List;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;

public class SparqlQueryParsingAndAssemblyTest {

    private static final String SIMPLE_QUERY = "SELECT ?x WHERE { ?x a ?type . }";

    private QueryParser queryParser;

    private QueryHolder sut;

    @BeforeEach
    void setUp() {
        this.queryParser = new Sparql11QueryParser(new ParameterValueFactory(mock(MetamodelProvider.class)));
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
}
