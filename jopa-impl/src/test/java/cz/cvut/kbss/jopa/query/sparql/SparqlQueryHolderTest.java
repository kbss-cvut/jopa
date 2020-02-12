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
package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;

class SparqlQueryHolderTest {

    private static final String QUERY = "SELECT ?x WHERE { ?x a ?type . }";
    private static final List<String> PARTS = Arrays.asList("SELECT ", " WHERE { ", " a ", " . }");
    private static final List<String> PARAMS = Arrays.asList("x", "x", "type");

    private SparqlQueryHolder sut;

    @BeforeEach
    void setUp() {
        this.sut = new SparqlQueryHolder(QUERY, PARTS,
                PARAMS.stream()
                      .map(name -> new QueryParameter<>(name, new ParameterValueFactory(mock(MetamodelProvider.class))))
                      .collect(Collectors.toList()));
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
