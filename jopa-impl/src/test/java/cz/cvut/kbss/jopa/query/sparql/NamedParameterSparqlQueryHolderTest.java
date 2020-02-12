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

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

public class NamedParameterSparqlQueryHolderTest {

    private static final String QUERY = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ?craft\n" +
            "{\n" +
            "?craft foaf:name \"Apollo 7\" .\n" +
            "?craft foaf:homepage ?homepage .\n" +
            "}";
    private static final List<String> PARTS = Arrays.asList("PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ", "\n{\n", " foaf:name \"Apollo 7\" .\n", " foaf:homepage ", " .\n}");
    private static final List<String> PARAMS = Arrays.asList("craft", "craft", "craft", "homepage");
    private static final Set<String> PARAM_NAMES = new HashSet<>(Arrays.asList("craft", "homepage"));

    private final ParameterValueFactory paramValueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));

    private SparqlQueryHolder holder;

    @BeforeEach
    public void setUp() throws Exception {
        final Map<String, QueryParameter<?>> paramsByName = new HashMap<>();
        for (String n : PARAM_NAMES) {
            paramsByName.put(n, new QueryParameter<>(n, paramValueFactory));
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
        final String value = "http://kbss.felk.cvut.cz";
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
        holder.setParameter(qp, URI.create("http://kbss.felk.cvut.cz"));
        assertNotNull(holder.getParameterValue(qp));
        holder.clearParameter(qp);
        assertNull(holder.getParameterValue(qp));
    }

    @Test
    public void clearParametersRemovesAllParameterValues() {
        final QueryParameter<?> qp = new QueryParameter<>("homepage", paramValueFactory);
        holder.setParameter(qp, URI.create("http://kbss.felk.cvut.cz"));
        final QueryParameter<?> qpTwo = new QueryParameter<>("craft", paramValueFactory);
        holder.setParameter(qpTwo, "Programming");
        holder.getParameters().forEach(param -> assertNotNull(holder.getParameterValue(param)));
        holder.clearParameters();
        holder.getParameters().forEach(param -> assertNull(holder.getParameterValue(param)));
    }

    @Test
    public void assembleQueryWithUri() {
        final QueryParameter<?> qp = new QueryParameter<>("homepage", paramValueFactory);
        holder.setParameter(qp, URI.create("http://kbss.felk.cvut.cz"));
        final String expected = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name \"Apollo 7\" .\n" +
                "?craft foaf:homepage <http://kbss.felk.cvut.cz> .\n" +
                "}";
        assertEquals(expected, holder.assembleQuery());
    }

    @Test
    public void assembleQueryWithLiteral() {
        final QueryParameter<?> qp = new QueryParameter<>("homepage", paramValueFactory);
        holder.setParameter(qp, "http://kbss.felk.cvut.cz", null);
        final String expected = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name \"Apollo 7\" .\n" +
                "?craft foaf:homepage \"http://kbss.felk.cvut.cz\" .\n" +
                "}";
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
    public void setParameterReplacesAllOccurrencesOfVariable() {
        final QueryParameter<?> qp = new QueryParameter<>("craft", paramValueFactory);
        holder.setParameter(qp, URI.create("http://kbss.felk.cvut.cz/apollo7"));
        final String result = holder.assembleQuery();
        assertFalse(result.contains("?craft"));
    }
}
