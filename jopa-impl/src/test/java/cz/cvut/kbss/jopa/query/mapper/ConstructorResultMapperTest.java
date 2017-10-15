package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exception.SparqlResultMappingException;
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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ConstructorResultMapperTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private ResultSet resultSetMock;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void mapRetrievesVariableValueAndUsesConstructorToCreateNewInstance() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper paramMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        when(paramMapper.map(resultSetMock)).thenReturn(uri);
        mapper.addParameterMapper(paramMapper);

        final Object result = mapper.map(resultSetMock);
        assertTrue(result instanceof OWLClassA);
        assertEquals(uri, ((OWLClassA) result).getUri());
        verify(paramMapper).map(resultSetMock);
    }

    @Test
    public void mapRetrievesValuesForMultipleConstructorParamsAndInstantiatesResult() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper idMapper = mock(VariableResultMapper.class);
        final VariableResultMapper stringMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        final String string = "stringAttributeValue";
        when(idMapper.map(resultSetMock)).thenReturn(uri);
        mapper.addParameterMapper(idMapper);
        when(stringMapper.map(resultSetMock)).thenReturn(string);
        mapper.addParameterMapper(stringMapper);

        final Object result = mapper.map(resultSetMock);
        assertTrue(result instanceof OWLClassA);
        assertEquals(uri, ((OWLClassA) result).getUri());
        assertEquals(string, ((OWLClassA) result).getStringAttribute());
    }

    @Test
    public void mapThrowsMappingExceptionWhenMatchingConstructorCannotBeFound() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(OWLClassA.class);
        final VariableResultMapper wrongMapper = mock(VariableResultMapper.class);
        when(wrongMapper.map(resultSetMock)).thenReturn(117);
        mapper.addParameterMapper(wrongMapper);
        thrown.expect(SparqlResultMappingException.class);
        final Object[] values = new Object[]{117};
        thrown.expectMessage(
                String.format("No matching constructor for values %s found in type %s.", Arrays.toString(values),
                        OWLClassA.class));

        mapper.map(resultSetMock);
    }

    @Test
    public void mapIsAbleToUsePrivateConstructorToCreateTargetInstance() {
        final ConstructorResultMapper mapper = new ConstructorResultMapper(WithPrivateConstructor.class);
        final VariableResultMapper wrongMapper = mock(VariableResultMapper.class);
        final URI uri = Generators.createIndividualIdentifier();
        when(wrongMapper.map(resultSetMock)).thenReturn(uri);
        mapper.addParameterMapper(wrongMapper);

        final Object result = mapper.map(resultSetMock);
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
        when(wrongMapper.map(resultSetMock)).thenReturn(uri);
        mapper.addParameterMapper(wrongMapper);
        thrown.expect(SparqlResultMappingException.class);
        thrown.expectCause(isA(InstantiationException.class));

        mapper.map(resultSetMock);
    }

    @SuppressWarnings("unused")
    private static abstract class AbstractClass {
        private URI uri;

        private AbstractClass(URI uri) {
            this.uri = uri;
        }
    }
}