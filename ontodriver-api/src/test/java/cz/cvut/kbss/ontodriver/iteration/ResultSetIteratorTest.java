/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.iteration;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.NoSuchElementException;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ResultSetIteratorTest {

    @Mock
    private ResultSet resultSet;

    private ResultSetIterator sut;

    @BeforeEach
    void setUp() {
        this.sut = new ResultSetIterator(resultSet);
    }

    @Test
    void hasNextDelegatesCallToResultSet() throws Exception {
        assertFalse(sut.hasNext());
        verify(resultSet).hasNext();
    }

    @Test
    void hasNextThrowsResultSetIterationExceptionWhenResultSetThrowsOntoDriverException() throws Exception {
        when(resultSet.hasNext()).thenThrow(OntoDriverException.class);
        final ResultSetIterationException result = assertThrows(ResultSetIterationException.class, () -> sut.hasNext());
        assertInstanceOf(OntoDriverException.class, result.getCause());
    }

    @Test
    void nextReturnsResultRowRepresentingCurrentResultSetRow() throws Exception {
        when(resultSet.hasNext()).thenReturn(true);
        final ResultRow row = sut.next();
        assertNotNull(row);
        assertInstanceOf(DelegatingResultRow.class, row);
    }

    @Test
    void nextInvokesNextOnUnderlyingResultSet() throws Exception {
        when(resultSet.hasNext()).thenReturn(true);
        sut.next();
        verify(resultSet).next();
    }

    @Test
    void nextThrowsResultSetIterationExceptionWhenResultSetThrowsOntoDriverException() throws Exception {
        when(resultSet.hasNext()).thenReturn(true);
        doThrow(OntoDriverException.class).when(resultSet).next();
        final ResultSetIterationException result = assertThrows(ResultSetIterationException.class, () -> sut.next());
        assertInstanceOf(OntoDriverException.class, result.getCause());
    }

    @Test
    void nextThrowsNoSuchElementExceptionWhenThereIsNoMoreElements() throws Exception {
        when(resultSet.hasNext()).thenReturn(false);
        assertThrows(NoSuchElementException.class, () -> sut.next());
    }
}
