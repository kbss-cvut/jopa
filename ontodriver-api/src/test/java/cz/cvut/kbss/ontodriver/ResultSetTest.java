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
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.iteration.ResultSetIterator;
import cz.cvut.kbss.ontodriver.iteration.ResultSetSpliterator;
import org.junit.jupiter.api.Test;

import java.util.Iterator;
import java.util.Spliterator;
import java.util.stream.Stream;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ResultSetTest {

    @Test
    void iteratorCreatesResultSetIteratorOverResultSetInstance() {
        final ResultSet sut = mock(ResultSet.class);
        when(sut.iterator()).thenCallRealMethod();
        when(sut.isOpen()).thenReturn(true);
        final Iterator<ResultRow> result = sut.iterator();
        assertNotNull(result);
        assertThat(result, instanceOf(ResultSetIterator.class));
    }

    @Test
    void iteratorThrowsIllegalStateWhenResultSetIsClosed() {
        final ResultSet sut = mock(ResultSet.class);
        when(sut.iterator()).thenCallRealMethod();
        when(sut.isOpen()).thenReturn(false);
        final IllegalStateException result = assertThrows(IllegalStateException.class, sut::iterator);
        assertThat(result.getMessage(), containsString("result set is closed"));
    }

    @Test
    void spliteratorCreatesResultSetSpliteratorOverResultSetInstance() {
        final ResultSet sut = mock(ResultSet.class);
        when(sut.spliterator()).thenCallRealMethod();
        when(sut.isOpen()).thenReturn(true);
        final Spliterator<ResultRow> result = sut.spliterator();
        assertThat(result, instanceOf(ResultSetSpliterator.class));
    }

    @Test
    void spliteratorThrowsIllegalStateExceptionWhenResultSetIsClosed() {
        final ResultSet sut = mock(ResultSet.class);
        when(sut.spliterator()).thenCallRealMethod();
        when(sut.isOpen()).thenReturn(false);
        final IllegalStateException result = assertThrows(IllegalStateException.class, sut::spliterator);
        assertThat(result.getMessage(), containsString("result set is closed"));
    }

    @Test
    void streamReturnsStreamOverResultSetInstance() {
        final ResultSet sut = mock(ResultSet.class);
        when(sut.spliterator()).thenCallRealMethod();
        when(sut.isOpen()).thenReturn(true);
        when(sut.stream()).thenCallRealMethod();
        final Stream<ResultRow> result = sut.stream();
        assertNotNull(result);
    }
}