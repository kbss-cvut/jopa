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
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.time.OffsetDateTime;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class VariableResultMapperTest {

    private static final String NAME = "x";

    @Mock
    private ResultRow resultRow;

    @Mock
    private UnitOfWork uowMock;

    @Test
    void mapReadsValueFromResultSetByVariableNameAndReturnsIt() throws OntoDriverException {
        final Integer value = 117;
        when(resultRow.isBound(NAME)).thenReturn(true);
        when(resultRow.getObject(NAME)).thenReturn(value);
        final VariableResultMapper mapper = new VariableResultMapper(NoType.getVariableMapping());
        final Object result = mapper.map(resultRow, uowMock);
        assertEquals(value, result);
        verify(resultRow).getObject(NAME);
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
    void mapCastsValueToTargetTypeWhenPossible() throws OntoDriverException {
        final Integer value = 117;
        when(resultRow.isBound(NAME)).thenReturn(true);
        when(resultRow.getObject(NAME)).thenReturn(value);
        final VariableResultMapper mapper = new VariableResultMapper(WithTypeCast.getVariableMapping());
        final Object result = mapper.map(resultRow, uowMock);
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
    void mapTransformsValueToTargetType() throws OntoDriverException {
        final String value = "http://onto.fel.cvut.cz";
        when(resultRow.isBound(NAME)).thenReturn(true);
        when(resultRow.getObject(NAME)).thenReturn(value);
        final VariableResultMapper mapper = new VariableResultMapper(WithTypeTransform.getVariableMapping());
        final Object result = mapper.map(resultRow, uowMock);
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
    void mapReturnsNullWhenVariableIsNotBoundInResultSetButIsNotRequired() throws Exception {
        when(resultRow.isBound(NAME)).thenReturn(false);
        final VariableResultMapper mapper = new VariableResultMapper(WithTypeTransform.getVariableMapping());
        final Object result = mapper.map(resultRow, uowMock);
        assertNull(result);
    }

    @Test
    void mapTransformsLangStringToString() throws OntoDriverException {
        final LangString value = new LangString("test", "en");
        when(resultRow.isBound(NAME)).thenReturn(true);
        when(resultRow.getObject(NAME)).thenReturn(value);
        final VariableResultMapper mapper = new VariableResultMapper(WithStringMapping.getVariableMapping());
        final Object result = mapper.map(resultRow, uowMock);
        assertEquals(value.getValue(), result);
    }

    @SparqlResultSetMapping(name = "testMapping", variables = {
            @VariableResult(name = NAME, type = String.class)
    })
    private static class WithStringMapping {

        private static VariableResult getVariableMapping() {
            return WithStringMapping.class.getDeclaredAnnotation(SparqlResultSetMapping.class).variables()[0];
        }
    }

    @Test
    void mapTransformsOffsetDateTimeToJavaUtilDate() throws OntoDriverException {
        final OffsetDateTime value = OffsetDateTime.now();
        when(resultRow.isBound(NAME)).thenReturn(true);
        when(resultRow.getObject(NAME)).thenReturn(value);
        final VariableResultMapper mapper = new VariableResultMapper(WithDateMapping.getVariableMapping());
        final Object result = mapper.map(resultRow, uowMock);
        assertEquals(new Date(value.toInstant().toEpochMilli()), result);
    }

    @SparqlResultSetMapping(name = "testMapping", variables = {
            @VariableResult(name = NAME, type = Date.class)
    })
    private static class WithDateMapping {

        private static VariableResult getVariableMapping() {
            return WithDateMapping.class.getDeclaredAnnotation(SparqlResultSetMapping.class).variables()[0];
        }
    }
}
