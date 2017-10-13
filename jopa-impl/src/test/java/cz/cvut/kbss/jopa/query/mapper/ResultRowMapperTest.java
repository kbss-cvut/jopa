package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.ontodriver.ResultSet;
import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mockito;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.mockito.Mockito.mock;

public class ResultRowMapperTest {

    private ResultRowMapper rowMapper = new ResultRowMapper("testMapping");

    @Test
    public void mapInvokesAllSubMappers() {
        final List<SparqlResultMapper> mappers = IntStream.range(0, 5).mapToObj(i -> mock(SparqlResultMapper.class))
                                                          .collect(Collectors.toList());
        mappers.forEach(m -> rowMapper.addMapper(m));
        final ResultSet resultSet = mock(ResultSet.class);
        rowMapper.map(resultSet);
        final InOrder inOrder = Mockito.inOrder(mappers.toArray());
        mappers.forEach(m -> inOrder.verify(m).map(resultSet));
    }
}