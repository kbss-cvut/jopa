/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.ontodriver.ResultSet;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Arrays;

import static org.hamcrest.core.Is.isA;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class ConstructorResultMapperTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private ResultSet resultSetMock;

    @Mock
    private UnitOfWorkImpl uowMock;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void mapRetrievesVariableValueAndUsesConstructorToCreateNewInstance() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper paramMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        when(paramMapper.map(resultSetMock, uowMock)).thenReturn(uri);
        mapper.addParameterMapper(paramMapper);

        final Object result = mapper.map(resultSetMock, uowMock);
        assertTrue(result instanceof OWLClassA);
        assertEquals(uri, ((OWLClassA) result).getUri());
        verify(paramMapper).map(resultSetMock, uowMock);
    }

    @Test
    public void mapRetrievesValuesForMultipleConstructorParamsAndInstantiatesResult() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper idMapper = mock(VariableResultMapper.class);
        final VariableResultMapper stringMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        final String string = "stringAttributeValue";
        when(idMapper.map(resultSetMock, uowMock)).thenReturn(uri);
        mapper.addParameterMapper(idMapper);
        when(stringMapper.map(resultSetMock, uowMock)).thenReturn(string);

        mapper.addParameterMapper(stringMapper);

        final Object result = mapper.map(resultSetMock, uowMock);
        assertTrue(result instanceof OWLClassA);
        assertEquals(uri, ((OWLClassA) result).getUri());
        assertEquals(string, ((OWLClassA) result).getStringAttribute());
    }

    @Test
    public void mapInstantiatesResultsWhenResultSetReturnsNullForVariableMapping() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper idMapper = mock(VariableResultMapper.class);
        final VariableResultMapper stringMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        when(idMapper.map(resultSetMock, uowMock)).thenReturn(uri);
        when(idMapper.getTargetType()).thenAnswer(inv -> URI.class);
        mapper.addParameterMapper(idMapper);
        when(stringMapper.map(resultSetMock, uowMock)).thenReturn(null);
        when(stringMapper.getTargetType()).thenAnswer(inv -> String.class);
        mapper.addParameterMapper(stringMapper);

        final Object result = mapper.map(resultSetMock, uowMock);
        assertTrue(result instanceof OWLClassA);
        assertEquals(uri, ((OWLClassA) result).getUri());
        assertNull(((OWLClassA) result).getStringAttribute());
    }

    @Test
    public void mapThrowsMappingExceptionWhenMatchingConstructorCannotBeFound() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper wrongMapper = mock(VariableResultMapper.class);
        when(wrongMapper.map(resultSetMock, uowMock)).thenReturn(117);
        mapper.addParameterMapper(wrongMapper);
        thrown.expect(SparqlResultMappingException.class);
        final Object[] values = new Object[]{117};
        thrown.expectMessage(
                String.format("No matching constructor for values %s found in type %s.", Arrays.toString(values),
                        OWLClassA.class));

        mapper.map(resultSetMock, uowMock);
    }

    @Test
    public void mapIsAbleToUsePrivateConstructorToCreateTargetInstance() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(WithPrivateConstructor.class);
        final VariableResultMapper wrongMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        when(wrongMapper.map(resultSetMock, uowMock)).thenReturn(uri);
        mapper.addParameterMapper(wrongMapper);

        final Object result = mapper.map(resultSetMock, uowMock);
        assertTrue(result instanceof WithPrivateConstructor);
        assertEquals(uri, ((WithPrivateConstructor) result).uri);
    }

    private static class WithPrivateConstructor {
        private URI uri;

        private WithPrivateConstructor(URI uri) {
            this.uri = uri;
        }
    }

    @Test
    public void mapThrowsMappingExceptionWhenItIsUnableToBuildTargetInstance() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(AbstractClass.class);
        final VariableResultMapper wrongMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        when(wrongMapper.map(resultSetMock, uowMock)).thenReturn(uri);
        mapper.addParameterMapper(wrongMapper);
        thrown.expect(SparqlResultMappingException.class);
        thrown.expectCause(isA(InstantiationException.class));

        mapper.map(resultSetMock, uowMock);
    }

    @SuppressWarnings("unused")
    private static abstract class AbstractClass {
        private URI uri;

        private AbstractClass(URI uri) {
            this.uri = uri;
        }
    }
}