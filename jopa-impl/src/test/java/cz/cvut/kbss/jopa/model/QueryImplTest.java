/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.anyVararg;
import static org.mockito.Mockito.*;

public class QueryImplTest extends QueryTestBase {

    private static final String SELECT_QUERY = "SELECT ?x ?y WHERE { ?x ?y ?z .}";

    @Override
    Query createQuery(String query, Class<?> resultType) {
        return queryFactory.createNativeQuery(query);
    }

    @Test
    public void getSingleResultReturnsUniqueResult() throws Exception {
        final Query q = queryFactory.createNativeQuery(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(false);
        when(resultSetMock.getObject(anyInt())).thenReturn("str");
        final Object[] result = (Object[]) q.getSingleResult();
        assertEquals(2, result.length); // Two variables
        assertEquals("str", result[0]);
        assertEquals("str", result[1]);
        verify(resultSetMock).next();
    }

    @Test(expected = NoUniqueResultException.class)
    public void getSingleResultWithMultipleResultsThrowsNoUniqueResultException() throws Exception {
        final Query q = queryFactory.createNativeQuery(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(true).thenReturn(false);
        when(resultSetMock.getObject(anyInt())).thenReturn("str");
        q.getSingleResult();
    }

    @Test
    public void setMaxResultsConstrainsNumberOfReturnedResults() throws Exception {
        final Query q = queryFactory.createNativeQuery(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        // Three results
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(true).thenReturn(true).thenReturn(false);
        when(resultSetMock.getObject(anyInt())).thenReturn("str");
        q.setMaxResults(2);
        final List result = q.getResultList();
        assertEquals(2, result.size());
    }

    @Test
    public void setMaxResultsToZeroReturnsImmediatelyEmptyResult() throws Exception {
        final Query q = queryFactory.createNativeQuery(SELECT_QUERY);
        q.setMaxResults(0);
        final List result = q.getResultList();
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(statementMock, never()).executeQuery(anyString(), anyVararg());
    }

    @Test
    public void queryResultRowIsArrayOfObjectsWhenMultipleColumnsExist() throws Exception {
        final Query q = queryFactory.createNativeQuery(SELECT_QUERY);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(false);
        final String res = "str";
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
        final Query q = queryFactory.createNativeQuery(SELECT_QUERY);
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
        final Query q = queryFactory.createNativeQuery(UPDATE_QUERY);
        q.executeUpdate();
        verify(statementMock).executeUpdate(UPDATE_QUERY);
    }

    @Test
    public void executeUpdateThrowsPersistenceExceptionWhenOntoDriverExceptionIsThrown() throws Exception {
        thrown.expect(OWLPersistenceException.class);
        thrown.expectMessage("Exception caught when evaluating query " + UPDATE_QUERY);
        doThrow(new OntoDriverException()).when(statementMock).executeUpdate(UPDATE_QUERY);
        final Query q = queryFactory.createNativeQuery(UPDATE_QUERY);
        q.executeUpdate();
    }
}