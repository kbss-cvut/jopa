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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Arrays;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ConstructorResultMapperTest {

    @Mock
    private ResultRow resultRow;

    @Mock
    private UnitOfWork uowMock;

    @Test
    void mapRetrievesVariableValueAndUsesConstructorToCreateNewInstance() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final URI uri = Generators.createIndividualIdentifier();
        final VariableResultMapper paramMapper = idMapper(uri);
        mapper.addParameterMapper(paramMapper);

        final Object result = mapper.map(resultRow, uowMock);
        assertInstanceOf(OWLClassA.class, result);
        assertEquals(uri, ((OWLClassA) result).getUri());
        verify(paramMapper).map(resultRow, uowMock);
    }

    private VariableResultMapper idMapper(URI uri) {
        final VariableResultMapper idMapper = mock(VariableResultMapper.class);
        when(idMapper.map(resultRow, uowMock)).thenReturn(uri);
        when(idMapper.getTargetType()).thenReturn((Class) URI.class);
        return idMapper;
    }

    @Test
    void mapRetrievesValuesForMultipleConstructorParamsAndInstantiatesResult() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final URI uri = Generators.createIndividualIdentifier();
        final VariableResultMapper idMapper = idMapper(uri);
        final VariableResultMapper stringMapper = mock(VariableResultMapper.class);
        final String string = "stringAttributeValue";
        mapper.addParameterMapper(idMapper);
        when(stringMapper.map(resultRow, uowMock)).thenReturn(string);
        when(stringMapper.getTargetType()).thenReturn((Class) String.class);

        mapper.addParameterMapper(stringMapper);

        final Object result = mapper.map(resultRow, uowMock);
        assertInstanceOf(OWLClassA.class, result);
        assertEquals(uri, ((OWLClassA) result).getUri());
        assertEquals(string, ((OWLClassA) result).getStringAttribute());
    }

    @Test
    void mapInstantiatesResultsWhenResultSetReturnsNullForVariableMapping() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final URI uri = Generators.createIndividualIdentifier();
        final VariableResultMapper idMapper = idMapper(uri);
        final VariableResultMapper stringMapper = mock(VariableResultMapper.class);
        mapper.addParameterMapper(idMapper);
        when(stringMapper.getTargetType()).thenAnswer(inv -> String.class);
        mapper.addParameterMapper(stringMapper);

        final Object result = mapper.map(resultRow, uowMock);
        assertInstanceOf(OWLClassA.class, result);
        assertEquals(uri, ((OWLClassA) result).getUri());
        assertNull(((OWLClassA) result).getStringAttribute());
    }

    @Test
    void mapThrowsMappingExceptionWhenMatchingConstructorCannotBeFound() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper wrongMapper = mock(VariableResultMapper.class);
        when(wrongMapper.map(resultRow, uowMock)).thenReturn(117);
        when(wrongMapper.getTargetType()).thenReturn((Class) Integer.class);
        mapper.addParameterMapper(wrongMapper);

        final SparqlResultMappingException result =
                assertThrows(SparqlResultMappingException.class, () -> mapper.map(resultRow, uowMock));
        final Object[] values = new Object[]{117};
        assertEquals(String.format("No matching constructor for values %s found in type %s.", Arrays.toString(values),
                OWLClassA.class), result.getMessage());
    }

    @Test
    void mapIsAbleToUsePrivateConstructorToCreateTargetInstance() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(WithPrivateConstructor.class);
        final URI uri = Generators.createIndividualIdentifier();
        final VariableResultMapper wrongMapper = idMapper(uri);
        mapper.addParameterMapper(wrongMapper);

        final Object result = mapper.map(resultRow, uowMock);
        assertInstanceOf(WithPrivateConstructor.class, result);
        assertEquals(uri, ((WithPrivateConstructor) result).uri);
    }

    private static class WithPrivateConstructor {
        private final URI uri;

        private WithPrivateConstructor(URI uri) {
            this.uri = uri;
        }
    }

    @Test
    void mapThrowsMappingExceptionWhenItIsUnableToBuildTargetInstance() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(AbstractClass.class);
        final URI uri = Generators.createIndividualIdentifier();
        final VariableResultMapper wrongMapper = idMapper(uri);
        when(wrongMapper.map(resultRow, uowMock)).thenReturn(uri);
        when(wrongMapper.getTargetType()).thenReturn((Class) URI.class);
        mapper.addParameterMapper(wrongMapper);

        final SparqlResultMappingException result =
                assertThrows(SparqlResultMappingException.class, () -> mapper.map(resultRow, uowMock));
        assertThat(result.getCause(), instanceOf(InstantiationException.class));
    }

    @SuppressWarnings("unused")
    private static abstract class AbstractClass {
        private final URI uri;

        private AbstractClass(URI uri) {
            this.uri = uri;
        }
    }

    @Test
    void mapSupportsAutomaticConversionFromLangStringToString() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final URI uri = Generators.createIndividualIdentifier();
        final VariableResultMapper idMapper = idMapper(uri);
        final VariableResultMapper stringMapper = mock(VariableResultMapper.class);
        mapper.addParameterMapper(idMapper);
        final LangString strValue = new LangString("test", "en");
        when(stringMapper.map(resultRow, uowMock)).thenReturn(strValue.getValue());
        when(stringMapper.getTargetType()).thenReturn((Class) String.class);
        mapper.addParameterMapper(stringMapper);

        final Object result = mapper.map(resultRow, uowMock);
        assertInstanceOf(OWLClassA.class, result);
        assertEquals(uri, ((OWLClassA) result).getUri());
        assertEquals(strValue.getValue(), ((OWLClassA) result).getStringAttribute());
    }

    @Test
    void mapPrefersDeclaredVariableTargetTypeOverActualValueTypeWhenRetrievingConstructor() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(GeneralConstructor.class);
        final URI uri = Generators.createIndividualIdentifier();
        final VariableResultMapper idMapper = idMapper(uri);
        final VariableResultMapper stringMapper = mock(VariableResultMapper.class);
        when(idMapper.map(resultRow, uowMock)).thenReturn(uri);
        when(idMapper.getTargetType()).thenReturn((Class) URI.class);
        mapper.addParameterMapper(idMapper);
        when(stringMapper.getTargetType()).thenReturn((Class) Object.class);
        when(stringMapper.map(resultRow, uowMock)).thenReturn("test");
        mapper.addParameterMapper(stringMapper);

        final Object result = mapper.map(resultRow, uowMock);
        assertInstanceOf(GeneralConstructor.class, result);
        assertEquals(uri, ((GeneralConstructor) result).uri);
        assertEquals("test", ((GeneralConstructor) result).str);
    }

    private static class GeneralConstructor {

        private final URI uri;
        private final Object str;

        GeneralConstructor(URI uri, Object str) {
            this.uri = uri;
            this.str = str;
        }
    }
}
