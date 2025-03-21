/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.anyInt;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class QueryImplTest extends QueryTestBase {

    @Override
    QueryImpl createQuery(String query, Class<?> resultType) {
        return createQuery(query);
    }

    @Override
    QueryImpl createQuery(String query) {
        final QueryImpl q = queryFactory.createNativeQuery(query);
        q.setEnsureOpenProcedure(ensureOpenProcedure);
        return q;
    }

    @Test
    void getSingleResultReturnsUniqueResult() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultRow.getColumnCount()).thenReturn(2);
        when(resultSetIterator.hasNext()).thenReturn(true).thenReturn(false);
        when(resultRow.isBound(anyInt())).thenReturn(true);
        when(resultRow.getObject(anyInt())).thenReturn("str");
        final Object[] result = (Object[]) q.getSingleResult();
        assertEquals(2, result.length); // Two variables
        assertEquals("str", result[0]);
        assertEquals("str", result[1]);
        verify(resultSetIterator).next();
    }

    @Test
    void getSingleResultWithMultipleResultsThrowsNoUniqueResultException() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultRow.getColumnCount()).thenReturn(2);
        when(resultSetIterator.hasNext()).thenReturn(true).thenReturn(true).thenReturn(false);
        when(resultRow.getObject(anyInt())).thenReturn("str");
        assertThrows(NoUniqueResultException.class, q::getSingleResult);
    }

    @Test
    void setMaxResultsExecutesQueryWithSpecifiedLimit() throws Exception {
        final Query sut = createQuery(SELECT_QUERY);
        final int maxResults = 2;
        sut.setMaxResults(maxResults).getResultList();
        final ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        verify(statementMock).executeQuery(captor.capture());
        assertThat(captor.getValue(), containsString("LIMIT " + maxResults));
    }

    @Test
    void queryResultRowIsArrayOfObjectsWhenMultipleColumnsExist() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultRow.getColumnCount()).thenReturn(2);
        when(resultSetIterator.hasNext()).thenReturn(true).thenReturn(false);
        final String res = "str";
        when(resultRow.isBound(anyInt())).thenReturn(true);
        when(resultRow.getObject(anyInt())).thenReturn(res);
        final List result = q.getResultList();
        for (Object row : result) {
            assertInstanceOf(Object[].class, row);
            final Object[] rowArr = (Object[]) row;
            for (Object o : rowArr) {
                assertEquals(res, o);
            }
        }
    }

    @Test
    void queryResultRowObjectWhenSingleColumnExists() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultRow.getColumnCount()).thenReturn(1);
        when(resultSetIterator.hasNext()).thenReturn(true).thenReturn(false);
        final String res = "str";
        when(resultRow.getObject(anyInt())).thenReturn(res);
        final List result = q.getResultList();
        for (Object o : result) {
            assertEquals(res, o);
        }
    }

    @Test
    void executeUpdateRunsUpdateOnConnection() throws Exception {
        final Query q = createQuery(UPDATE_QUERY);
        q.executeUpdate();
        verify(statementMock).executeUpdate(UPDATE_QUERY);
    }

    @Test
    void executeUpdateThrowsPersistenceExceptionWhenOntoDriverExceptionIsThrown() throws Exception {
        doThrow(new OntoDriverException()).when(statementMock).executeUpdate(UPDATE_QUERY);
        final Query q = createQuery(UPDATE_QUERY);
        final OWLPersistenceException result = assertThrows(OWLPersistenceException.class, q::executeUpdate);
        assertEquals("Exception caught when evaluating query " + UPDATE_QUERY, result.getMessage());
    }

    @Test
    void exceptionInExecuteUpdateInvokesRollbackMarker() throws Exception {
        doThrow(new OntoDriverException()).when(statementMock).executeUpdate(UPDATE_QUERY);
        final QueryImpl q = createQuery(UPDATE_QUERY);
        runAndVerifyHandlerInvocation(q, q::executeUpdate);
    }

    private void runAndVerifyHandlerInvocation(QueryImpl query, Runnable method) {
        query.setRollbackOnlyMarker(handler);
        try {
            method.run();
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void runtimeExceptionInExecuteUpdateInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeUpdate(UPDATE_QUERY);
        final QueryImpl q = createQuery(UPDATE_QUERY);
        runAndVerifyHandlerInvocation(q, q::executeUpdate);
    }

    @Test
    void exceptionInGetResultListInvokesRollbackMarker() throws Exception {
        doThrow(OntoDriverException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final QueryImpl q = createQuery(SELECT_QUERY);
        runAndVerifyHandlerInvocation(q, q::getResultList);
    }

    @Test
    void runtimeExceptionInGetResultListInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final QueryImpl q = createQuery(SELECT_QUERY);
        runAndVerifyHandlerInvocation(q, q::getResultList);
    }

    @Test
    void exceptionInGetSingleResultInvokesRollbackMarker() throws Exception {
        doThrow(OntoDriverException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final QueryImpl q = createQuery(SELECT_QUERY);
        runAndVerifyHandlerInvocation(q, q::getSingleResult);
    }

    @Test
    void runtimeExceptionInGetSingleResultInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final QueryImpl q = createQuery(SELECT_QUERY);
        runAndVerifyHandlerInvocation(q, q::getSingleResult);
    }

    @Test
    void exceptionInSetMaxResultsInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setMaxResults(-1);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    private QueryImpl queryWithRollbackMarker(String query) {
        final QueryImpl q = createQuery(query);
        q.setRollbackOnlyMarker(handler);
        return q;
    }

    @Test
    void exceptionInSetParameterByPositionInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter(117, 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void exceptionInSetStringParameterByPositionInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter(117, "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void exceptionInSetParameterByNameInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter("a", 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void exceptionInSetStringParameterByNameInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter("a", "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void exceptionInSetParameterByParameterInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter(new QueryParameter<>(117, mock(ParameterValueFactory.class)), 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void exceptionInSetStringParameterByParameterInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter(new QueryParameter<>(117, mock(ParameterValueFactory.class)), "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).run();
    }

    @Test
    void setFirstResultExecutesQueryWithSpecifiedOffset() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        final int firstResult = 2;
        q.setFirstResult(firstResult).getResultList();
        final ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        verify(statementMock).executeQuery(captor.capture());
        assertThat(captor.getValue(), containsString("OFFSET " + firstResult));
    }

    @Test
    void getResultListSetsValuesOfUnboundVariablesToNullInResultArrays() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultRow.getColumnCount()).thenReturn(2);
        when(resultSetIterator.hasNext()).thenReturn(true).thenReturn(false);
        when(resultRow.isBound(0)).thenReturn(true);
        when(resultRow.isBound(1)).thenReturn(false);
        when(resultRow.getObject(0)).thenReturn("str");
        final List result = q.getResultList();
        assertEquals(1, result.size());
        assertEquals("str", ((Object[]) result.get(0))[0]);
        assertNull(((Object[]) result.get(0))[1]);
        verify(resultRow).isBound(0);
        verify(resultRow).isBound(1);
        verify(resultRow, never()).getObject(1);
    }

    @Test
    void noUniqueResultExceptionInGetSingleResultDoesNotCauseTransactionRollback() throws Exception {
        final Query query = queryWithRollbackMarker(SELECT_QUERY);
        when(resultRow.getColumnCount()).thenReturn(2);
        when(resultSetIterator.hasNext()).thenReturn(true).thenReturn(true).thenReturn(false);
        when(resultRow.getObject(anyInt())).thenReturn("str");
        assertThrows(NoUniqueResultException.class, query::getSingleResult);
        verify(handler, never()).run();
    }

    @Test
    void noResultExceptionInGetSingleResultDoesNotCauseTransactionRollback() {
        final Query query = queryWithRollbackMarker(SELECT_QUERY);
        assertThrows(NoResultException.class, query::getSingleResult);
        verify(handler, never()).run();
    }

    @SuppressWarnings("unchecked")
    @Test
    void getResultStreamRetrievesResultStreamFromUnderlyingResultSet() throws Exception {
        final Query sut = createQuery(SELECT_QUERY);
        when(resultSetMock.isOpen()).thenReturn(true);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.isBound(anyInt())).thenReturn(true);
        when(resultSetMock.hasNext()).thenReturn(true, true, false);
        when(resultSetMock.getObject(anyInt())).thenReturn("str");
        final Stream result = sut.getResultStream();
        assertNotNull(result);
        final List asList = (List) result.collect(Collectors.toList());
        assertEquals(2, asList.size());
        final Object[] expected = new Object[]{"str", "str"};
        asList.forEach(item -> assertArrayEquals(expected, (Object[]) item));
    }

    @SuppressWarnings("unchecked")
    @Test
    void getResultStreamClosesStatementWhenStreamIsProcessed() throws Exception {
        final Query sut = createQuery(SELECT_QUERY);
        when(resultSetMock.isOpen()).thenReturn(true);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.isBound(anyInt())).thenReturn(true);
        when(resultSetMock.hasNext()).thenReturn(true, true, false);
        when(resultSetMock.getObject(anyInt())).thenReturn("str");
        sut.getResultStream().forEach(Assertions::assertNotNull);
        verify(statementMock).close();
    }

    @SuppressWarnings("unchecked")
    @Test
    void getResultStreamClosesStatementWhenStreamProcessingThrowsException() throws Exception {
        final Query sut = createQuery(SELECT_QUERY);
        when(resultSetMock.isOpen()).thenReturn(true);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.isBound(anyInt())).thenReturn(true);
        when(resultSetMock.hasNext()).thenReturn(true, true, false);
        when(resultSetMock.getObject(anyInt())).thenThrow(OntoDriverException.class);
        try {
            assertThrows(OWLPersistenceException.class, () -> sut.getResultStream().forEach(Assertions::assertNotNull));
        } finally {
            verify(statementMock).close();
        }
    }

    @SuppressWarnings("unchecked")
    @Test
    void getResultStreamClosesResultSetWhenStreamIsProcessed() throws Exception {
        final Query sut = createQuery(SELECT_QUERY);
        when(resultSetMock.isOpen()).thenReturn(true);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.isBound(anyInt())).thenReturn(true);
        when(resultSetMock.hasNext()).thenReturn(true, true, false);
        when(resultSetMock.getObject(anyInt())).thenReturn("str");
        try (Stream<Object> res = sut.getResultStream()) {
            final Optional<Object> any = res.findAny();
            assertTrue(any.isPresent());
        } finally {
            verify(statementMock).close();
            verify(resultSetMock).close();
        }
    }
}
