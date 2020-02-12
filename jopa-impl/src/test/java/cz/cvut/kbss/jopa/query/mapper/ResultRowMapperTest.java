/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
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

class ResultRowMapperTest {

    @Mock
    private ResultRow resultRow;

    @Mock
    private UnitOfWorkImpl uowMock;

    private ResultRowMapper rowMapper = new ResultRowMapper("testMapping");

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void mapInvokesAllSubMappers() {
        final List<SparqlResultMapper> mappers = IntStream.range(0, 5).mapToObj(i -> mock(SparqlResultMapper.class))
                                                          .collect(Collectors.toList());
        mappers.forEach(m -> rowMapper.addMapper(m));
        rowMapper.map(resultRow, uowMock);
        final InOrder inOrder = Mockito.inOrder(mappers.toArray());
        mappers.forEach(m -> inOrder.verify(m).map(resultRow, uowMock));
    }

    @Test
    void mapReturnsOneValueWhenOneMapperIsConfigured() {
        final SparqlResultMapper mapper = mock(SparqlResultMapper.class);
        rowMapper.addMapper(mapper);
        final OWLClassA instance = Generators.generateOwlClassAInstance();
        when(mapper.map(resultRow, uowMock)).thenReturn(instance);
        final Object result = rowMapper.map(resultRow, uowMock);
        assertSame(instance, result);
    }
}