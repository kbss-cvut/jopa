/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
import cz.cvut.kbss.ontodriver.exception.OntoDriverRuntimeException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Spliterator;
import java.util.function.Consumer;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@SuppressWarnings("unchecked")
class ResultSetSpliteratorTest {

    @Mock
    private ResultSet resultSet;

    private ResultSetSpliterator sut;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        this.sut = new ResultSetSpliterator(resultSet);
    }

    @Test
    void constructorCreatesSpliteratorWithOrderedNonNullAndImmutableCharacteristics() {
        assertTrue(sut.hasCharacteristics(Spliterator.IMMUTABLE));
        assertTrue(sut.hasCharacteristics(Spliterator.ORDERED));
        assertTrue(sut.hasCharacteristics(Spliterator.NONNULL));
        assertFalse(sut.hasCharacteristics(Spliterator.CONCURRENT));
        assertFalse(sut.hasCharacteristics(Spliterator.DISTINCT));
        assertFalse(sut.hasCharacteristics(Spliterator.SIZED));
        assertFalse(sut.hasCharacteristics(Spliterator.SUBSIZED));
        assertFalse(sut.hasCharacteristics(Spliterator.SORTED));
    }

    /**
     * This is according to {@link java.util.Spliterators.AbstractSpliterator} specs, where {@link Long#MAX_VALUE}
     * should be reported for spliterators with unknown estimate size.
     */
    @Test
    void constructorReportsMaxLongValueEstimateSize() {
        assertEquals(Long.MAX_VALUE, sut.estimateSize());
    }

    @Test
    void tryAdvanceInvokesActionOnNextResultRowAndReturnsTrue() throws Exception {
        when(resultSet.hasNext()).thenReturn(true);
        final Consumer<ResultRow> consumer = mock(Consumer.class);
        final boolean result = sut.tryAdvance(consumer);
        assertTrue(result);
        verify(resultSet).next();
        verify(consumer).accept(any(ResultRow.class));
    }

    @Test
    void tryAdvanceDoesNothingAndReturnsFalseWhenThereAreNoMoreRowsInResultSet() throws Exception {
        when(resultSet.hasNext()).thenReturn(false);
        final Consumer<ResultRow> consumer = mock(Consumer.class);
        final boolean result = sut.tryAdvance(consumer);
        assertFalse(result);
        verify(resultSet, never()).next();
        verify(consumer, never()).accept(any(ResultRow.class));
    }

    @Test
    void tryAdvanceThrowsOntoDriverRuntimeExceptionWhenResultResultSetThrowsException() throws Exception {
        when(resultSet.hasNext()).thenReturn(true);
        doThrow(OntoDriverException.class).when(resultSet).next();
        final Consumer<ResultRow> consumer = mock(Consumer.class);
        final OntoDriverRuntimeException result =
                assertThrows(OntoDriverRuntimeException.class, () -> sut.tryAdvance(consumer));
        assertThat(result.getCause(), instanceOf(OntoDriverException.class));
    }

    @Test
    void forEachRemainingInvokesConsumerForAllResultSetRows() throws Exception {
        when(resultSet.hasNext()).thenReturn(true).thenReturn(true).thenReturn(true).thenReturn(false);
        final Consumer<ResultRow> consumer = mock(Consumer.class);
        sut.forEachRemaining(consumer);
        verify(resultSet, times(3)).next();
        verify(consumer, times(3)).accept(any(ResultRow.class));
    }

    @Test
    void forEachRemainingThrowsOntoDriverRuntimeExceptionWhenResultSetThrowsException() throws Exception {
        when(resultSet.hasNext()).thenReturn(true);
        doThrow(OntoDriverException.class).when(resultSet).next();
        final Consumer<ResultRow> consumer = mock(Consumer.class);
        final OntoDriverRuntimeException result =
                assertThrows(OntoDriverRuntimeException.class, () -> sut.forEachRemaining(consumer));
        assertThat(result.getCause(), instanceOf(OntoDriverException.class));
    }
}