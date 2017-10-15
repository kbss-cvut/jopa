package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.ontodriver.ResultSet;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ResultRowMapperTest {

    @Mock
    private ResultSet resultSetMock;

    private ResultRowMapper rowMapper = new ResultRowMapper("testMapping");

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void mapInvokesAllSubMappers() {
        final List<SparqlResultMapper> mappers = IntStream.range(0, 5).mapToObj(i -> mock(SparqlResultMapper.class))
                                                          .collect(Collectors.toList());
        mappers.forEach(m -> rowMapper.addMapper(m));
        rowMapper.map(resultSetMock);
        final InOrder inOrder = Mockito.inOrder(mappers.toArray());
        mappers.forEach(m -> inOrder.verify(m).map(resultSetMock));
    }

    @Test
    public void mapReturnsOneValueWhenOneMapperIsConfigured() {
        final SparqlResultMapper mapper = mock(SparqlResultMapper.class);
        rowMapper.addMapper(mapper);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        when(mapper.map(resultSetMock)).thenReturn(instance);
        final Object result = rowMapper.map(resultSetMock);
        assertSame(instance, result);
    }
}