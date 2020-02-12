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
package cz.cvut.kbss.ontodriver.iteration;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.NoSuchElementException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ResultSetIteratorTest {

    @Mock
    private ResultSet resultSet;

    private ResultSetIterator sut;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
        this.sut = new ResultSetIterator(resultSet);
    }

    @Test
    void hasNextDelegatesCallToResultSet() throws Exception {
        sut.hasNext();
        verify(resultSet).hasNext();
    }

    @Test
    void hasNextThrowsResultSetIterationExceptionWhenResultSetThrowsOntoDriverException() throws Exception {
        when(resultSet.hasNext()).thenThrow(OntoDriverException.class);
        final ResultSetIterationException result = assertThrows(ResultSetIterationException.class, () -> sut.hasNext());
        assertTrue(result.getCause() instanceof OntoDriverException);
    }

    @Test
    void nextReturnsResultRowRepresentingCurrentResultSetRow() throws Exception {
        when(resultSet.hasNext()).thenReturn(true);
        final ResultRow row = sut.next();
        assertNotNull(row);
        assertTrue(row instanceof DelegatingResultRow);
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
        assertTrue(result.getCause() instanceof OntoDriverException);
    }

    @Test
    void nextThrowsNoSuchElementExceptionWhenThereIsNoMoreElements() throws Exception {
        when(resultSet.hasNext()).thenReturn(false);
        assertThrows(NoSuchElementException.class, () -> sut.next());
    }
}