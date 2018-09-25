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
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.model.annotations.EntityResult;
import cz.cvut.kbss.jopa.model.annotations.FieldResult;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.*;

public class FieldResultMapperTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private ResultSet resultSetMock;

    @Mock
    private UnitOfWork uowMock;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    @SparqlResultSetMapping(name = "test", entities = {
            @EntityResult(entityClass = WithMapping.class, fields = {
                    @FieldResult(name = "a", variable = "ax")
            })
    })
    private static class WithMapping {

        private static FieldResult getFieldResult() {
            return WithMapping.class.getDeclaredAnnotation(SparqlResultSetMapping.class).entities()[0].fields()[0];
        }
    }

    @Test
    public void mapThrowsResultMappingExceptionWhenValueCannotBeAssignedToField() throws Exception {
        final FieldResult fieldResult = WithMapping.getFieldResult();
        final FieldSpecification fsMock = mock(FieldSpecification.class);
        when(fsMock.getJavaType()).thenReturn(Boolean.class);
        when(fsMock.getJavaField()).thenReturn(OWLClassM.getBooleanAttributeField());
        when(resultSetMock.isBound(fieldResult.variable())).thenReturn(true);
        when(resultSetMock.getObject(fieldResult.variable())).thenReturn("string");
        thrown.expect(SparqlResultMappingException.class);
        thrown.expectMessage(containsString(
                "Value " + resultSetMock.getObject(fieldResult.variable()) + " cannot be assigned to field of type " +
                        fsMock.getJavaType()));

        final FieldResultMapper mapper = new FieldResultMapper(fieldResult, fsMock);
        final OWLClassM target = new OWLClassM();
        mapper.map(resultSetMock, target, uowMock);
        assertNull(target.getBooleanAttribute());
    }

    @Test
    public void mapExtractsValueFromResultSetAndSetsItOnTargetObjectField() throws Exception {
        final FieldResult fieldResult = WithMapping.getFieldResult();
        final FieldSpecification fsMock = mock(FieldSpecification.class);
        when(fsMock.getJavaType()).thenReturn(String.class);
        when(fsMock.getJavaField()).thenReturn(OWLClassA.getStrAttField());
        final String value = "stringValue";
        when(resultSetMock.isBound(fieldResult.variable())).thenReturn(true);
        when(resultSetMock.getObject(fieldResult.variable())).thenReturn(value);

        final OWLClassA target = new OWLClassA();
        final FieldResultMapper mapper = new FieldResultMapper(fieldResult, fsMock);
        mapper.map(resultSetMock, target, uowMock);
        verify(resultSetMock).getObject(fieldResult.variable());
        assertEquals(value, target.getStringAttribute());
    }

    @Test
    public void mapSkipsVariablesNotPresentInResultSet() throws Exception {
        final FieldResult fieldResult = WithMapping.getFieldResult();
        final FieldSpecification fsMock = mock(FieldSpecification.class);
        when(fsMock.getJavaType()).thenReturn(Boolean.class);
        final OntoDriverException e = new OntoDriverException(
                "Result set does not contain column " + fieldResult.variable() + ".");
        when(resultSetMock.getObject(fieldResult.variable())).thenThrow(e);

        final FieldResultMapper mapper = new FieldResultMapper(fieldResult, fsMock);
        final OWLClassM target = new OWLClassM();
        mapper.map(resultSetMock, target, uowMock);
        assertNull(target.getBooleanAttribute());
    }

    @Test
    public void mapSkipsFieldForWhichVariableHasNoBindingInCurrentResultRow() throws Exception {
        final FieldResult fieldResult = WithMapping.getFieldResult();
        final FieldSpecification fsMock = mock(FieldSpecification.class);
        when(fsMock.getJavaType()).thenReturn(Boolean.class);
        when(resultSetMock.getObject(fieldResult.variable())).thenReturn(null);

        final FieldResultMapper mapper = new FieldResultMapper(fieldResult, fsMock);
        final OWLClassM target = new OWLClassM();
        mapper.map(resultSetMock, target, uowMock);
        assertNull(target.getBooleanAttribute());
    }
}