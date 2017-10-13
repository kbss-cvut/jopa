package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
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

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void mapReadsValueFromResultSetByVariableNameAndReturnsIt() throws OntoDriverException {
        final Integer value = 117;
        when(resultSet.getObject(NAME)).thenReturn(value);
        final VariableResultMapper mapper = new VariableResultMapper(NoType.getVariableMapping());
        final Object result = mapper.map(resultSet);
        assertEquals(value, result);
        verify(resultSet).getObject(NAME);
    }

    @SparqlResultSetMapping(variables = {
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
        final Object result = mapper.map(resultSet);
        assertTrue(result instanceof Number);
        assertEquals(value, result);
    }

    @SparqlResultSetMapping(variables = {
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
        final Object result = mapper.map(resultSet);
        assertTrue(result instanceof URI);
        assertEquals(URI.create(value), result);
    }

    @SparqlResultSetMapping(variables = {
            @VariableResult(name = NAME, type = URI.class)
    })
    private static class WithTypeTransform {

        private static VariableResult getVariableMapping() {
            return WithTypeTransform.class.getDeclaredAnnotation(SparqlResultSetMapping.class).variables()[0];
        }
    }

    @Test
    public void mapThrowsPersistenceExceptionWhenVariableIsNotFoundInResult() throws OntoDriverException {
        final String message = "No result binding found for label" + NAME;
        when(resultSet.getObject(NAME)).thenThrow(new OntoDriverException(message));
        thrown.expect(OWLPersistenceException.class);
        thrown.expectMessage(message);
        final VariableResultMapper mapper = new VariableResultMapper(WithTypeTransform.getVariableMapping());
        mapper.map(resultSet);
    }
}