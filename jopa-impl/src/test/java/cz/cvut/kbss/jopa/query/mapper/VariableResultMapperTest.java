/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class VariableResultMapperTest {

    private static final String NAME = "x";

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private ResultSet resultSet;

    @Mock
    private UnitOfWorkImpl uowMock;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void mapReadsValueFromResultSetByVariableNameAndReturnsIt() throws OntoDriverException {
        final Integer value = 117;
        when(resultSet.getObject(NAME)).thenReturn(value);
        final VariableResultMapper mapper = new VariableResultMapper(NoType.getVariableMapping());
        final Object result = mapper.map(resultSet, uowMock);
        assertEquals(value, result);
        verify(resultSet).getObject(NAME);
    }

    @SparqlResultSetMapping(name = "testMapping", variables = {
            @VariableResult(name = NAME)
    })
    private static class NoType {

        private static VariableResult getVariableMapping() {
            return NoType.class.getDeclaredAnnotation(SparqlResultSetMapping.class).variables()[0];
        }
    }

    @Test
    public void mapCastsValueToTargetTypeWhenPossible() throws OntoDriverException {
        final Integer value = 117;
        when(resultSet.getObject(NAME)).thenReturn(value);
        final VariableResultMapper mapper = new VariableResultMapper(WithTypeCast.getVariableMapping());
        final Object result = mapper.map(resultSet, uowMock);
        assertTrue(result instanceof Number);
        assertEquals(value, result);
    }

    @SparqlResultSetMapping(name = "testMapping", variables = {
            @VariableResult(name = NAME, type = Number.class)
    })
    private static class WithTypeCast {

        private static VariableResult getVariableMapping() {
            return WithTypeCast.class.getDeclaredAnnotation(SparqlResultSetMapping.class).variables()[0];
        }
    }

    @Test
    public void mapTransformsValueToTargetType() throws OntoDriverException {
        final String value = "http://onto.fel.cvut.cz";
        when(resultSet.getObject(NAME)).thenReturn(value);
        final VariableResultMapper mapper = new VariableResultMapper(WithTypeTransform.getVariableMapping());
        final Object result = mapper.map(resultSet, uowMock);
        assertTrue(result instanceof URI);
        assertEquals(URI.create(value), result);
    }

    @SparqlResultSetMapping(name = "testMapping", variables = {
            @VariableResult(name = NAME, type = URI.class)
    })
    private static class WithTypeTransform {

        private static VariableResult getVariableMapping() {
            return WithTypeTransform.class.getDeclaredAnnotation(SparqlResultSetMapping.class).variables()[0];
        }
    }

    @Test
    public void mapThrowsMappingExceptionWhenVariableIsNotFoundInResult() throws OntoDriverException {
        final String message = "No result binding found for label" + NAME;
        when(resultSet.getObject(NAME)).thenThrow(new OntoDriverException(message));
        thrown.expect(SparqlResultMappingException.class);
        thrown.expectMessage(message);
        final VariableResultMapper mapper = new VariableResultMapper(WithTypeTransform.getVariableMapping());
        mapper.map(resultSet, uowMock);
    }
}