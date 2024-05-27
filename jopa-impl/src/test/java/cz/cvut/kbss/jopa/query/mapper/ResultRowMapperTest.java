/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ResultRowMapperTest {

    @Mock
    private ResultRow resultRow;

    @Mock
    private UnitOfWork uowMock;

    private final ResultRowMapper rowMapper = new ResultRowMapper("testMapping");

    @Test
    void mapInvokesAllSubMappers() {
        final List<SparqlResultMapper> mappers = IntStream.range(0, 5).mapToObj(i -> mock(SparqlResultMapper.class))
                                                          .collect(Collectors.toList());
        mappers.forEach(rowMapper::addMapper);
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
