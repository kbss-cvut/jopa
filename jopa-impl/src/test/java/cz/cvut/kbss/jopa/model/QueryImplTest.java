/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.*;

public class QueryImplTest extends QueryTestBase {

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
    public void getSingleResultReturnsUniqueResult() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(false);
        when(resultSetMock.isBound(anyInt())).thenReturn(true);
        when(resultSetMock.getObject(anyInt())).thenReturn("str");
        final Object[] result = (Object[]) q.getSingleResult();
        assertEquals(2, result.length); // Two variables
        assertEquals("str", result[0]);
        assertEquals("str", result[1]);
        verify(resultSetMock).next();
    }

    @Test(expected = NoUniqueResultException.class)
    public void getSingleResultWithMultipleResultsThrowsNoUniqueResultException() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(true).thenReturn(false);
        when(resultSetMock.getObject(anyInt())).thenReturn("str");
        q.getSingleResult();
    }

    @Test
    public void setMaxResultsConstrainsNumberOfReturnedResults() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        // Three results
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(true).thenReturn(true).thenReturn(false);
        when(resultSetMock.getObject(anyInt())).thenReturn("str");
        final int expectedCount = 2;
        q.setMaxResults(expectedCount);
        assertEquals(expectedCount, q.getMaxResults());
        final List result = q.getResultList();
        assertEquals(expectedCount, result.size());
    }

    @Test
    public void setMaxResultsToZeroReturnsImmediatelyEmptyResult() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        q.setMaxResults(0);
        final List result = q.getResultList();
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(statementMock, never()).executeQuery(anyString());
    }

    @Test
    public void queryResultRowIsArrayOfObjectsWhenMultipleColumnsExist() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(false);
        final String res = "str";
        when(resultSetMock.isBound(anyInt())).thenReturn(true);
        when(resultSetMock.getObject(anyInt())).thenReturn(res);
        final List result = q.getResultList();
        for (Object row : result) {
            assertTrue(row instanceof Object[]);
            final Object[] rowArr = (Object[]) row;
            for (Object o : rowArr) {
                assertEquals(res, o);
            }
        }
    }

    @Test
    public void queryResultRowObjectWhenSingleColumnExists() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(1);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(false);
        final String res = "str";
        when(resultSetMock.getObject(anyInt())).thenReturn(res);
        final List result = q.getResultList();
        for (Object o : result) {
            assertEquals(res, o);
        }
    }

    @Test
    public void executeUpdateRunsUpdateOnConnection() throws Exception {
        final Query q = createQuery(UPDATE_QUERY);
        q.executeUpdate();
        verify(statementMock).executeUpdate(UPDATE_QUERY);
    }

    @Test
    public void executeUpdateThrowsPersistenceExceptionWhenOntoDriverExceptionIsThrown() throws Exception {
        thrown.expect(OWLPersistenceException.class);
        thrown.expectMessage("Exception caught when evaluating query " + UPDATE_QUERY);
        doThrow(new OntoDriverException()).when(statementMock).executeUpdate(UPDATE_QUERY);
        final Query q = createQuery(UPDATE_QUERY);
        q.executeUpdate();
    }

    @Test
    public void exceptionInExecuteUpdateInvokesRollbackMarker() throws Exception {
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
        verify(handler).execute();
    }

    @Test
    public void runtimeExceptionInExecuteUpdateInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeUpdate(UPDATE_QUERY);
        final QueryImpl q = createQuery(UPDATE_QUERY);
        runAndVerifyHandlerInvocation(q, q::executeUpdate);
    }

    @Test
    public void exceptionInGetResultListInvokesRollbackMarker() throws Exception {
        doThrow(OntoDriverException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final QueryImpl q = createQuery(SELECT_QUERY);
        runAndVerifyHandlerInvocation(q, q::getResultList);
    }

    @Test
    public void runtimeExceptionInGetResultListInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final QueryImpl q = createQuery(SELECT_QUERY);
        runAndVerifyHandlerInvocation(q, q::getResultList);
    }

    @Test
    public void exceptionInGetSingleResultInvokesRollbackMarker() throws Exception {
        doThrow(OntoDriverException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final QueryImpl q = createQuery(SELECT_QUERY);
        runAndVerifyHandlerInvocation(q, q::getSingleResult);
    }

    @Test
    public void runtimeExceptionInGetSingleResultInvokesRollbackMarker() throws Exception {
        doThrow(OWLPersistenceException.class).when(statementMock).executeQuery(SELECT_QUERY);
        final QueryImpl q = createQuery(SELECT_QUERY);
        runAndVerifyHandlerInvocation(q, q::getSingleResult);
    }

    @Test
    public void exceptionInSetMaxResultsInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setMaxResults(-1);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    private QueryImpl queryWithRollbackMarker(String query) {
        final QueryImpl q = createQuery(query);
        q.setRollbackOnlyMarker(handler);
        return q;
    }

    @Test
    public void exceptionInSetParameterByPositionInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter(117, 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void exceptionInSetStringParameterByPositionInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter(117, "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void exceptionInSetParameterByNameInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter("a", 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void exceptionInSetStringParameterByNameInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter("a", "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void exceptionInSetParameterByParameterInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter(new QueryParameter<>(117), 117);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void exceptionInSetStringParameterByParameterInvokesRollbackMarker() {
        final QueryImpl q = queryWithRollbackMarker(SELECT_QUERY);
        try {
            q.setParameter(new QueryParameter<>(117), "A", "en");
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        verify(handler).execute();
    }

    @Test
    public void setFirstResultOffsetsQueryResultStartToSpecifiedPosition() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(1);
        // Three results
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(true).thenReturn(true).thenReturn(false);
        when(resultSetMock.getObject(anyInt())).thenReturn("str");
        q.setFirstResult(2);
        assertEquals(2, q.getFirstResult());
        final List result = q.getResultList();
        assertEquals(1, result.size());
        assertEquals("str", result.get(0));
        verify(resultSetMock).getObject(anyInt());
    }

    @Test
    public void getResultListSetsValuesOfUnboundVariablesToNullInResultArrays() throws Exception {
        final Query q = createQuery(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(false);
        when(resultSetMock.isBound(0)).thenReturn(true);
        when(resultSetMock.isBound(1)).thenReturn(false);
        when(resultSetMock.getObject(0)).thenReturn("str");
        final List result = q.getResultList();
        assertEquals(1, result.size());
        assertEquals("str", ((Object[]) result.get(0))[0]);
        assertNull(((Object[]) result.get(0))[1]);
        verify(resultSetMock).isBound(0);
        verify(resultSetMock).isBound(1);
        verify(resultSetMock, never()).getObject(1);
    }

    @Test
    public void noUniqueResultExceptionInGetSingleResultDoesNotCauseTransactionRollback() throws Exception {
        final Query query = queryWithRollbackMarker(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(true).thenReturn(false);
        when(resultSetMock.getObject(anyInt())).thenReturn("str");
        thrown.expect(NoUniqueResultException.class);
        try {
            query.getSingleResult();
        } finally {
            verify(handler, never()).execute();
        }
    }

    @Test
    public void noResultExceptionInGetSingleResultDoesNotCauseTransactionRollback() {
        final Query query = queryWithRollbackMarker(SELECT_QUERY);
        thrown.expect(NoResultException.class);
        try {
            query.getSingleResult();
        } finally {
            verify(handler, never()).execute();
        }
    }
}