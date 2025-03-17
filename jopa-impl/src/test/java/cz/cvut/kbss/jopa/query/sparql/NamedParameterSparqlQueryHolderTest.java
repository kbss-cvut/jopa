/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.endsWith;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

public class NamedParameterSparqlQueryHolderTest {

    private static final String QUERY = """
            PREFIX foaf: <http://xmlns.com/foaf/0.1/>
            SELECT ?craft
            {
              ?craft foaf:name "Apollo 7" .
              ?craft foaf:homepage ?homepage .
            }""";
    private static final List<String> PARTS = Arrays.asList("PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ", "\n{\n  ", " foaf:name \"Apollo 7\" .\n  ", " foaf:homepage ", " .\n}");
    private static final List<String> PARAMS = Arrays.asList("craft", "craft", "craft", "homepage");
    private static final Set<String> PARAM_NAMES = new HashSet<>(Arrays.asList("craft", "homepage"));

    private final ParameterValueFactory paramValueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));

    private SparqlQueryHolder holder;

    @BeforeEach
    public void setUp() {
        final Map<String, QueryParameter<?>> paramsByName = new HashMap<>();
        for (String n : PARAM_NAMES) {
            final QueryParameter<?> qp = new QueryParameter<>(n, paramValueFactory);
            if ("craft".equals(n)) {
                qp.setProjected(true);
            }
            paramsByName.put(n, qp);
        }
        final List<QueryParameter<?>> parameters = PARAMS.stream().map(paramsByName::get).collect(Collectors.toList());
        this.holder = new SparqlQueryHolder(QUERY, PARTS, parameters);
    }

    @Test
    public void testGetQuery() {
        final String original = holder.getQuery();
        assertEquals(QUERY, original);
    }

    @Test
    public void testGetParameters() {
        final Collection<Parameter<?>> res = holder.getParameters();
        assertEquals(PARAM_NAMES.size(), res.size());
        for (Parameter<?> p : res) {
            assertTrue(PARAM_NAMES.contains(p.getName()));
        }
    }

    @Test
    public void testGetParameterByName() {
        final String name = PARAM_NAMES.iterator().next();
        final Parameter<?> p = holder.getParameter(name);
        assertNotNull(p);
        assertEquals(name, p.getName());
    }

    @Test
    public void getParameterByUnknownNameThrowsIllegalArgument() {
        assertThrows(IllegalArgumentException.class, () -> holder.getParameter("stupidUnknownParameter"));
    }

    @Test
    public void getValueOfUnknownParameterThrowsIllegalArgumentException() {
        final Parameter<Object> p = new QueryParameter<>("homepage", paramValueFactory);
        holder.setParameter(p, 1);
        assertThrows(IllegalArgumentException.class,
                () -> holder.getParameterValue(new QueryParameter<>("blabla", paramValueFactory)));
    }

    @Test
    public void testSetParameter() {
        final String value = "https://kbss.felk.cvut.cz";
        holder.setParameter(new QueryParameter<>("homepage", paramValueFactory), value);
        assertEquals(value, holder.getParameterValue(holder.getParameter("homepage")));
    }

    @Test
    public void setParameterToNullThrowsException() {
        assertThrows(NullPointerException.class,
                () -> holder.setParameter(new QueryParameter<>("homepage", paramValueFactory), null));
    }

    @Test
    public void setNullParameterThrowsException() {
        assertThrows(NullPointerException.class, () -> holder.setParameter(null, "Whatever"));
    }

    @Test
    public void setUnknownParameterThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> holder
                .setParameter(new QueryParameter<>("unknown", paramValueFactory), "Whatever"));
    }

    @Test
    public void clearParameterRemovesParameterValue() {
        final QueryParameter<?> qp = new QueryParameter<>("homepage", paramValueFactory);
        holder.setParameter(qp, URI.create("https://kbss.felk.cvut.cz"));
        assertNotNull(holder.getParameterValue(qp));
        holder.clearParameter(qp);
        assertNull(holder.getParameterValue(qp));
    }

    @Test
    public void clearParametersRemovesAllParameterValues() {
        final QueryParameter<?> qp = new QueryParameter<>("homepage", paramValueFactory);
        holder.setParameter(qp, URI.create("https://kbss.felk.cvut.cz"));
        final QueryParameter<?> qpTwo = new QueryParameter<>("craft", paramValueFactory);
        holder.setParameter(qpTwo, "Programming");
        holder.getParameters().forEach(param -> assertNotNull(holder.getParameterValue(param)));
        holder.clearParameters();
        holder.getParameters().forEach(param -> assertNull(holder.getParameterValue(param)));
    }

    @Test
    public void assembleQueryWithUri() {
        final QueryParameter<?> qp = new QueryParameter<>("homepage", paramValueFactory);
        holder.setParameter(qp, URI.create("https://kbss.felk.cvut.cz"));
        final String expected = """
                PREFIX foaf: <http://xmlns.com/foaf/0.1/>
                SELECT ?craft
                {
                  ?craft foaf:name "Apollo 7" .
                  ?craft foaf:homepage <https://kbss.felk.cvut.cz> .
                }""";
        assertEquals(expected, holder.assembleQuery());
    }

    @Test
    public void assembleQueryWithLiteral() {
        final QueryParameter<?> qp = new QueryParameter<>("homepage", paramValueFactory);
        holder.setParameter(qp, "https://kbss.felk.cvut.cz", null);
        final String expected = """
                PREFIX foaf: <http://xmlns.com/foaf/0.1/>
                SELECT ?craft
                {
                  ?craft foaf:name "Apollo 7" .
                  ?craft foaf:homepage "https://kbss.felk.cvut.cz" .
                }""";
        assertEquals(expected, holder.assembleQuery());
    }

    @Test
    public void setParametersAndAssembleQueryWithMultipleParamsNextToEachOther() {
        final ParameterValueFactory vf = paramValueFactory;
        final String query = "SELECT ?y ?z WHERE { <http://krizik.felk.cvut.cz/ontologies/jopa#entityA> ?y ?z . }";
        final List<QueryParameter<?>> params = Arrays
                .asList(new QueryParameter<>("y", vf), new QueryParameter<>("z", vf),
                        new QueryParameter<>("x", vf),
                        new QueryParameter<>("y", vf),
                        new QueryParameter<>("z", vf));
        final List<String> parts = Arrays.asList("SELECT ", " ", " WHERE { ", " ", " ", " . }");
        this.holder = new SparqlQueryHolder(query, parts, params);
        holder.setParameter(new QueryParameter<>("x", vf),
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityA"));
        final String result = holder.assembleQuery();
        assertEquals(query, result);
    }

    @Test
    void setParameterAddsValuesClauseWhenSetParameterIsInProjection() {
        final QueryParameter<?> qp = new QueryParameter<>("craft", paramValueFactory);
        qp.setProjected(true);
        holder.setParameter(qp, URI.create("https://kbss.felk.cvut.cz/apollo7"));
        final String result = holder.assembleQuery();
        assertThat(result, endsWith("VALUES (?craft) { ( " + holder.getParameter("craft").getValue()
                .getQueryString() + " ) }"));
        assertThat(result, containsString(QUERY));
    }
}
