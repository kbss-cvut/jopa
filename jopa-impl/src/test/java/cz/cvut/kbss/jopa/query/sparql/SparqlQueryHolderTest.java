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

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;

class SparqlQueryHolderTest {

    private static final String QUERY = "SELECT ?x WHERE { ?x a ?type . }";

    private SparqlQueryHolder sut;

    @BeforeEach
    void setUp() {
        final List<QueryParameter<?>> queryParams = new ArrayList<>();
        final ParameterValueFactory parameterValueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));
        final QueryParameter<URI> xParam = new QueryParameter<>("x", parameterValueFactory);
        xParam.setProjected(true);
        queryParams.add(xParam);
        queryParams.add(xParam);
        queryParams.add(new QueryParameter<>("type", parameterValueFactory));
        this.sut = new SparqlQueryHolder(QUERY, List.of("SELECT ", " WHERE { ", " a ", " . }"), queryParams);
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

    @Test
    void assembleQuerySupportsPluralValues() {
        final List<URI> paramValues = List.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier());
        sut.setParameter(sut.getParameter("x"), paramValues);
        final String result = sut.assembleQuery();
        assertThat(result, containsString("VALUES (?x)"));
        paramValues.forEach(v -> assertThat(result, containsString(" ( " + IdentifierTransformer.stringifyIri(v) + " )")));
    }

    @Test
    void assembleQuerySupportsValuesWithInEqualSize() {
        final List<QueryParameter<?>> queryParams = new ArrayList<>();
        final ParameterValueFactory parameterValueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));
        final QueryParameter<URI> xParam = new QueryParameter<>("x", parameterValueFactory);
        xParam.setProjected(true);
        queryParams.add(xParam);
        final QueryParameter<URI> typeParam = new QueryParameter<>("type", parameterValueFactory);
        typeParam.setProjected(true);
        queryParams.add(typeParam);
        queryParams.add(xParam);
        queryParams.add(typeParam);
        this.sut = new SparqlQueryHolder(QUERY, List.of("SELECT ", ", ", " WHERE { ", " a ", " . }"), queryParams);
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
