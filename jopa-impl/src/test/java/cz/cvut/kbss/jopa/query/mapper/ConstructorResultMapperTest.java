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
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Arrays;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ConstructorResultMapperTest {

    @Mock
    private ResultRow resultRow;

    @Mock
    private UnitOfWorkImpl uowMock;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void mapRetrievesVariableValueAndUsesConstructorToCreateNewInstance() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper paramMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        when(paramMapper.map(resultRow, uowMock)).thenReturn(uri);
        mapper.addParameterMapper(paramMapper);

        final Object result = mapper.map(resultRow, uowMock);
        assertTrue(result instanceof OWLClassA);
        assertEquals(uri, ((OWLClassA) result).getUri());
        verify(paramMapper).map(resultRow, uowMock);
    }

    @Test
    void mapRetrievesValuesForMultipleConstructorParamsAndInstantiatesResult() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper idMapper = mock(VariableResultMapper.class);
        final VariableResultMapper stringMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        final String string = "stringAttributeValue";
        when(idMapper.map(resultRow, uowMock)).thenReturn(uri);
        mapper.addParameterMapper(idMapper);
        when(stringMapper.map(resultRow, uowMock)).thenReturn(string);

        mapper.addParameterMapper(stringMapper);

        final Object result = mapper.map(resultRow, uowMock);
        assertTrue(result instanceof OWLClassA);
        assertEquals(uri, ((OWLClassA) result).getUri());
        assertEquals(string, ((OWLClassA) result).getStringAttribute());
    }

    @Test
    void mapInstantiatesResultsWhenResultSetReturnsNullForVariableMapping() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper idMapper = mock(VariableResultMapper.class);
        final VariableResultMapper stringMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        when(idMapper.map(resultRow, uowMock)).thenReturn(uri);
        when(idMapper.getTargetType()).thenAnswer(inv -> URI.class);
        mapper.addParameterMapper(idMapper);
        when(stringMapper.map(resultRow, uowMock)).thenReturn(null);
        when(stringMapper.getTargetType()).thenAnswer(inv -> String.class);
        mapper.addParameterMapper(stringMapper);

        final Object result = mapper.map(resultRow, uowMock);
        assertTrue(result instanceof OWLClassA);
        assertEquals(uri, ((OWLClassA) result).getUri());
        assertNull(((OWLClassA) result).getStringAttribute());
    }

    @Test
    void mapThrowsMappingExceptionWhenMatchingConstructorCannotBeFound() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper wrongMapper = mock(VariableResultMapper.class);
        when(wrongMapper.map(resultRow, uowMock)).thenReturn(117);
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
        final VariableResultMapper wrongMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        when(wrongMapper.map(resultRow, uowMock)).thenReturn(uri);
        mapper.addParameterMapper(wrongMapper);

        final Object result = mapper.map(resultRow, uowMock);
        assertTrue(result instanceof WithPrivateConstructor);
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
        final VariableResultMapper wrongMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        when(wrongMapper.map(resultRow, uowMock)).thenReturn(uri);
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
        final VariableResultMapper idMapper = mock(VariableResultMapper.class);
        final VariableResultMapper stringMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        when(idMapper.map(resultRow, uowMock)).thenReturn(uri);
        when(idMapper.getTargetType()).thenAnswer(inv -> URI.class);
        mapper.addParameterMapper(idMapper);
        final LangString strValue = new LangString("test", "en");
        when(stringMapper.map(resultRow, uowMock)).thenReturn(strValue.getValue());
        when(stringMapper.getTargetType()).thenAnswer(inv -> String.class);
        mapper.addParameterMapper(stringMapper);

        final Object result = mapper.map(resultRow, uowMock);
        assertTrue(result instanceof OWLClassA);
        assertEquals(uri, ((OWLClassA) result).getUri());
        assertEquals(strValue.getValue(), ((OWLClassA) result).getStringAttribute());
    }
}
