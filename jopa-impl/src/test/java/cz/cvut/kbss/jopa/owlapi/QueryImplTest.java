package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.query.Query;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.anyVararg;
import static org.mockito.Mockito.*;

public class QueryImplTest extends QueryTestBase {

    @Override
    Query<?> createQuery(String query, Class<?> resultType) {
        return queryFactory.createNativeQuery(query);
    }

    @Test
    public void getSingleResultReturnsUniqueResult() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<List<String>> q = queryFactory.createNativeQuery(query);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(false);
        when(resultSetMock.getString(anyInt())).thenReturn("str");
        final List<String> result = q.getSingleResult();
        assertEquals(2, result.size()); // Two variables
        assertEquals("str", result.get(0));
        assertEquals("str", result.get(1));
        verify(resultSetMock).next();
    }

    @Test(expected = NoUniqueResultException.class)
    public void getSingleResultWithMultipleResultsThrowsNoUniqueResultException() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<List<String>> q = queryFactory.createNativeQuery(query);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(true).thenReturn(false);
        when(resultSetMock.getString(anyInt())).thenReturn("str");
        q.getSingleResult();
    }

    @Test
    public void setMaxResultsConstrainsNumberOfReturnedResults() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<List<String>> q = queryFactory.createNativeQuery(query);
        when(resultSetMock.getColumnCount()).thenReturn(2);
        // Three results
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(true).thenReturn(true).thenReturn(false);
        when(resultSetMock.getString(anyInt())).thenReturn("str");
        q.setMaxResults(2);
        final List<List<String>> result = q.getResultList();
        assertEquals(2, result.size());
    }

    @Test
    public void setMaxResultsToZeroReturnsImmediatelyEmptyResult() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<List<String>> q = queryFactory.createNativeQuery(query);
        q.setMaxResults(0);
        final List<List<String>> result = q.getResultList();
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(statementMock, never()).executeQuery(anyString(), anyVararg());
    }
}