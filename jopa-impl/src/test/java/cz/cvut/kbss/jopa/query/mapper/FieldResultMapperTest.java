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
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
import cz.cvut.kbss.jopa.model.annotations.EntityResult;
import cz.cvut.kbss.jopa.model.annotations.FieldResult;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class FieldResultMapperTest {

    @Mock
    private ResultRow resultRow;

    @Mock
    private UnitOfWork uowMock;

    @BeforeEach
    void setUp() {
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
    void mapThrowsResultMappingExceptionWhenValueCannotBeAssignedToField() throws Exception {
        final FieldResult fieldResult = WithMapping.getFieldResult();
        final FieldSpecification fsMock = mock(FieldSpecification.class);
        when(fsMock.getJavaType()).thenReturn(Boolean.class);
        when(fsMock.getJavaField()).thenReturn(OWLClassM.getBooleanAttributeField());
        when(resultRow.isBound(fieldResult.variable())).thenReturn(true);
        when(resultRow.getObject(fieldResult.variable())).thenReturn(117);

        final FieldResultMapper mapper = new FieldResultMapper(fieldResult, fsMock);
        final OWLClassM target = new OWLClassM();
        final SparqlResultMappingException result =
                assertThrows(SparqlResultMappingException.class, () -> mapper.map(resultRow, target, uowMock));
        assertThat(result.getMessage(), containsString(
                "Value " + resultRow
                        .getObject(fieldResult.variable()) + " cannot be assigned (or transformed) to field of type " +
                        fsMock.getJavaType()));
        assertNull(target.getBooleanAttribute());
    }

    @Test
    void mapExtractsValueFromResultSetAndSetsItOnTargetObjectField() throws Exception {
        final FieldResult fieldResult = WithMapping.getFieldResult();
        final FieldSpecification fsMock = mock(FieldSpecification.class);
        when(fsMock.getJavaType()).thenReturn(String.class);
        when(fsMock.getJavaField()).thenReturn(OWLClassA.getStrAttField());
        final String value = "stringValue";
        when(resultRow.isBound(fieldResult.variable())).thenReturn(true);
        when(resultRow.getObject(fieldResult.variable())).thenReturn(value);

        final OWLClassA target = new OWLClassA();
        final FieldResultMapper mapper = new FieldResultMapper(fieldResult, fsMock);
        mapper.map(resultRow, target, uowMock);
        verify(resultRow).getObject(fieldResult.variable());
        assertEquals(value, target.getStringAttribute());
    }

    @Test
    void mapSkipsVariablesNotPresentInResultSet() throws Exception {
        final FieldResult fieldResult = WithMapping.getFieldResult();
        final FieldSpecification fsMock = mock(FieldSpecification.class);
        when(fsMock.getJavaType()).thenReturn(Boolean.class);
        final OntoDriverException e = new OntoDriverException(
                "Result set does not contain column " + fieldResult.variable() + ".");
        when(resultRow.getObject(fieldResult.variable())).thenThrow(e);

        final FieldResultMapper mapper = new FieldResultMapper(fieldResult, fsMock);
        final OWLClassM target = new OWLClassM();
        mapper.map(resultRow, target, uowMock);
        assertNull(target.getBooleanAttribute());
    }

    @Test
    void mapSkipsFieldForWhichVariableHasNoBindingInCurrentResultRow() throws Exception {
        final FieldResult fieldResult = WithMapping.getFieldResult();
        final FieldSpecification fsMock = mock(FieldSpecification.class);
        when(fsMock.getJavaType()).thenReturn(Boolean.class);
        when(resultRow.getObject(fieldResult.variable())).thenReturn(null);

        final FieldResultMapper mapper = new FieldResultMapper(fieldResult, fsMock);
        final OWLClassM target = new OWLClassM();
        mapper.map(resultRow, target, uowMock);
        assertNull(target.getBooleanAttribute());
    }

    @Test
    void mapSupportsTransformationFromLangStringToStringField() throws Exception {
        final FieldResult fieldResult = WithMapping.getFieldResult();
        final FieldSpecification fsMock = mock(FieldSpecification.class);
        when(fsMock.getJavaType()).thenReturn(String.class);
        when(fsMock.getJavaField()).thenReturn(OWLClassA.getStrAttField());
        final LangString value = new LangString("test", "en");
        when(resultRow.isBound(fieldResult.variable())).thenReturn(true);
        when(resultRow.getObject(fieldResult.variable())).thenReturn(value);

        final FieldResultMapper mapper = new FieldResultMapper(fieldResult, fsMock);
        final OWLClassA target = new OWLClassA();
        mapper.map(resultRow, target, uowMock);
        assertEquals(value.getValue(), target.getStringAttribute());
    }
}
