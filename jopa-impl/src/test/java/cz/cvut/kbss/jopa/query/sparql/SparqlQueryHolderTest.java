package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.query.QueryParameter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;

class SparqlQueryHolderTest {

    private static final String QUERY = "SELECT ?x WHERE { ?x a ?type . }";
    private static final List<String> PARTS = Arrays.asList("SELECT ", " WHERE { ", " a ", " . }");
    private static final List<String> PARAMS = Arrays.asList("x", "x", "type");

    private SparqlQueryHolder sut;

    @BeforeEach
    void setUp() {
        this.sut = new SparqlQueryHolder(QUERY, PARTS,
                PARAMS.stream().map(QueryParameter::new).collect(Collectors.toList()));
    }

    @Test
    void setMaxResultsAddsLimitClauseToAssembledQuery() {
        sut.setMaxResults(10);
        final String result = sut.assembleQuery();
        assertThat(result, endsWith("LIMIT 10"));
    }

    @Test
    void assembleDoesNotAddLimitClauseWhenMaxResultsWasNotSet() {
        final String result = sut.assembleQuery();
        assertThat(result, not(containsString("LIMIT")));
    }

    @Test
    void setFirstResultAddsOffsetClauseToAssembledQuery() {
        sut.setFirstResult(5);
        final String result = sut.assembleQuery();
        assertThat(result, endsWith("OFFSET 5"));
    }

    @Test
    void assembleDoesNotAddOffsetClauseWhenFirstResultWasNotSet() {
        final String result = sut.assembleQuery();
        assertThat(result, not(containsString("OFFSET")));
    }

    @Test
    void setFirstResultAndSetMaxResultsAddsLimitAndOffsetToAssembledQuery() {
        sut.setFirstResult(5);
        sut.setMaxResults(10);
        final String result = sut.assembleQuery();
        assertThat(result, containsString("LIMIT 10"));
        assertThat(result, containsString("OFFSET 5"));
    }
}