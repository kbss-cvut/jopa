package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class VariableResultMapperTest {

    private static final String NAME = "x";

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
}