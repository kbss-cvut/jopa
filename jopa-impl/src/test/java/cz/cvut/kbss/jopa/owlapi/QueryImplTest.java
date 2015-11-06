package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryFactory;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.QueryFactory;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.ontodriver_new.ResultSet;
import cz.cvut.kbss.ontodriver_new.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.anyVararg;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public class QueryImplTest {

    @Mock
    private ConnectionWrapper connectionWrapperMock;
    @Mock
    private UnitOfWorkImpl uowMock;
    @Mock
    private Statement statementMock;
    @Mock
    private ResultSet resultSetMock;

    private QueryFactory queryFactory;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(connectionWrapperMock.createStatement()).thenReturn(statementMock);
        when(statementMock.executeQuery(anyString(), anyVararg())).thenReturn(resultSetMock);
        this.queryFactory = new SparqlQueryFactory(uowMock, connectionWrapperMock);
    }

    @Test
    public void getResultListWithoutParameterSettingJustPassesTheOriginalQuery() throws Exception {
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z .}";
        final Query<?> q = queryFactory.createNativeQuery(query);
        q.getResultList();
        verify(connectionWrapperMock).createStatement();
        verify(statementMock).executeQuery(query);
    }

    @Test
    public void setParameterByNameSetsAllOccurrencesOfVariableInQuery() throws Exception {
        final String query = "SELECT ?y ?z WHERE { ?x ?y ?z . ?z ?y ?x . }";
        final Query<?> q = queryFactory.createNativeQuery(query);
        q.setParameter("x", "Individual");
        q.getResultList();
        verify(connectionWrapperMock).createStatement();
        verify(statementMock).executeQuery("SELECT ?y ?z WHERE { \"Individual\" ?y ?z . ?z ?y \"Individual\" . }");
    }

    @Test
    public void setParameterByParameterSetsValueWithCorrectType() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = queryFactory.createNativeQuery(query);
        final Parameter<URI> p = (Parameter<URI>) q.getParameter("z");
        q.setParameter(p, URI.create("http://krizik.felk.cvut.cz"));
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y <http://krizik.felk.cvut.cz> .}");
    }

    @Test
    public void testSetStringParameterWithLanguageTag() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = queryFactory.createNativeQuery(query);
        q.setParameter("z", "Object", "en");
        assertEquals("Object", q.getParameterValue("z"));
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y \"Object\"@en .}");
    }

    @Test
    public void testSetStringParameterWithLanguageTagUsingParameterMethod() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = queryFactory.createNativeQuery(query);
        final Parameter<String> p = (Parameter<String>) q.getParameter("z");
        q.setParameter(p, "Object", "cs");
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y \"Object\"@cs .}");
    }

    @Test(expected = IllegalStateException.class)
    public void getParameterValueThrowsIllegalStateForUnboundParam() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = queryFactory.createNativeQuery(query);
        q.getParameterValue("z");
    }

    @Test
    public void isBoundIndicatesWhetherVariableHasBeenBound() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = queryFactory.createNativeQuery(query);
        final Parameter<?> p = q.getParameter("z");
        assertFalse(q.isBound(p));
        q.setParameter(p.getName(), "Test");
        assertTrue(q.isBound(p));
        assertEquals("Test", q.getParameterValue(p.getName()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void setMaxResultsToLessThanZeroThrowsIllegalArgument() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = queryFactory.createNativeQuery(query);
        q.setMaxResults(-1);
    }

    @Test
    public void addingContextsPassesThemToQueryEvaluation() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = queryFactory.createNativeQuery(query);
        q.addContext(URI.create("http://contextOne"));
        q.addContexts(Arrays.asList(URI.create("http://contextTwo"), URI.create("http://contextThree")));
        q.getResultList();
        final ArgumentCaptor<URI[]> captor = ArgumentCaptor.forClass(URI[].class);
        verify(statementMock).executeQuery(eq(query), captor.capture());
        final List<URI[]> contexts = captor.getAllValues();
        assertEquals(3, contexts.size());
        // Mockito interprets the varargs as separate arguments
        assertTrue(contexts.contains(URI.create("http://contextOne")));
        assertTrue(contexts.contains(URI.create("http://contextTwo")));
        assertTrue(contexts.contains(URI.create("http://contextThree")));
    }

    @Test(expected = NoResultException.class)
    public void getSingleResultWithoutResultThrowsNoResultException() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = queryFactory.createNativeQuery(query);
        q.getSingleResult();
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