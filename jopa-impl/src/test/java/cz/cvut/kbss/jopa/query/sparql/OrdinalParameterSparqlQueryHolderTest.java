/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

public class OrdinalParameterSparqlQueryHolderTest {

    private static final String QUERY = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ?craft\n" +
            "{\n" +
            "?craft foaf:name \"Apollo 7\" .\n" +
            "?craft foaf:homepage $1 .\n" +
            "?craft foaf:fundedBy $ .\n" +
            "}";
    private static final List<String> PARTS = Arrays.asList("PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ", "\n{\n", " foaf:name \"Apollo 7\" .\n", " foaf:homepage ", " .\n", " foaf:fundedBy ", " .\n}");
    private static final List<Object> PARAMS = Arrays.asList("craft", "craft", "craft", 1, "craft", 2);
    private static final Set<Object> PARAM_IDENTIFIERS = new HashSet<>(Arrays.asList("craft", 1, 2));

    private final ParameterValueFactory paramValueFactory = new ParameterValueFactory(mock(MetamodelProvider.class));

    private SparqlQueryHolder holder;

    @BeforeEach
    public void setUp() {
        final Map<Object, QueryParameter<?>> paramsByName = new HashMap<>();
        for (Object n : PARAM_IDENTIFIERS) {
            if (n instanceof String) {
                paramsByName.put(n, new QueryParameter<>((String) n, paramValueFactory));
            } else {
                paramsByName.put(n, new QueryParameter<>((Integer) n, paramValueFactory));
            }
        }
        final List<QueryParameter<?>> parameters = PARAMS.stream().map(paramsByName::get).collect(Collectors.toList());
        this.holder = new SparqlQueryHolder(QUERY, PARTS, parameters);
    }

    @Test
    public void testGetOrdinalParameter() {
        final int position = 1;
        final Parameter<?> param = holder.getParameter(position);
        assertEquals(position, param.getPosition().intValue());
    }

    @Test
    public void getParameterWithUnknownIndexThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> holder.getParameter(Integer.MAX_VALUE));
    }

    @Test
    public void testSetOrdinalParameterValue() {
        final String value = "NASA";
        final QueryParameter<?> param = new QueryParameter<>(2, paramValueFactory);
        holder.setParameter(param, value);
        assertEquals(value, holder.getParameterValue(param));
    }

    @Test
    public void setValueOfUnknownParameterThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> holder
                .setParameter(new QueryParameter<>(Integer.MAX_VALUE, paramValueFactory), "value"));
    }

    @Test
    public void testSetAllParametersAndAssembleQueryWithPositionalParameters() {
        final String fundedBy = "NASA";
        final String homepage = "http://www.apollo-7.com";
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name \"Apollo 7\" .\n" +
                "?craft foaf:homepage \"" + homepage + "\" .\n" +
                "?craft foaf:fundedBy \"" + fundedBy + "\" .\n" +
                "}";
        holder.setParameter(new QueryParameter<>(1, paramValueFactory), homepage);
        holder.setParameter(new QueryParameter<>(2, paramValueFactory), fundedBy);
        assertEquals(query, holder.assembleQuery());
    }

    @Test
    public void testSetSomeParametersAndAssembleQueryWithPositionalParameters() {
        final String fundedBy = "NASA";
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
                "SELECT ?craft\n" +
                "{\n" +
                "?craft foaf:name \"Apollo 7\" .\n" +
                "?craft foaf:homepage $1 .\n" +
                "?craft foaf:fundedBy \"" + fundedBy + "\" .\n" +
                "}";
        holder.setParameter(new QueryParameter<>(2, paramValueFactory), fundedBy);
        assertEquals(query, holder.assembleQuery());
    }
}
