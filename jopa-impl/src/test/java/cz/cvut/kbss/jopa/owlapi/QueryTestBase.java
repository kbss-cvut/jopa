package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryFactory;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.QueryFactory;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public abstract class QueryTestBase {

    @Mock
    ConnectionWrapper connectionWrapperMock;
    @Mock
    UnitOfWorkImpl uowMock;
    @Mock
    Statement statementMock;
    @Mock
    ResultSet resultSetMock;

    QueryFactory queryFactory;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(connectionWrapperMock.createStatement()).thenReturn(statementMock);
        when(statementMock.executeQuery(anyString(), anyVararg())).thenReturn(resultSetMock);
        this.queryFactory = new SparqlQueryFactory(uowMock, connectionWrapperMock);
    }

    abstract Query<?> createQuery(String query, Class<?> resultType);

    @Test
    public void getResultListWithoutParameterSettingJustPassesTheOriginalQuery() throws Exception {
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z .}";
        final Query<?> q = createQuery(query, Object.class);
        q.getResultList();
        verify(connectionWrapperMock).createStatement();
        verify(statementMock).executeQuery(query);
    }

    @Test
    public void setParameterByNameSetsAllOccurrencesOfVariableInQuery() throws Exception {
        final String query = "SELECT ?y ?z WHERE { ?x ?y ?z . ?z ?y ?x . }";
        final Query<?> q = createQuery(query, Object.class);
        q.setParameter("x", "Individual");
        q.getResultList();
        verify(connectionWrapperMock).createStatement();
        verify(statementMock).executeQuery("SELECT ?y ?z WHERE { \"Individual\" ?y ?z . ?z ?y \"Individual\" . }");
    }

    @Test
    public void setParameterByParameterSetsValueWithCorrectType() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = createQuery(query, Object.class);
        final Parameter<URI> p = (Parameter<URI>) q.getParameter("z");
        q.setParameter(p, URI.create("http://krizik.felk.cvut.cz"));
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y <http://krizik.felk.cvut.cz> .}");
    }

    @Test
    public void testSetStringParameterWithLanguageTag() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = createQuery(query, Object.class);
        q.setParameter("z", "Object", "en");
        assertEquals("Object", q.getParameterValue("z"));
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y \"Object\"@en .}");
    }

    @Test
    public void testSetStringParameterWithLanguageTagUsingParameterMethod() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = createQuery(query, Object.class);
        final Parameter<String> p = (Parameter<String>) q.getParameter("z");
        q.setParameter(p, "Object", "cs");
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y \"Object\"@cs .}");
    }

    @Test(expected = IllegalStateException.class)
    public void getParameterValueThrowsIllegalStateForUnboundParam() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = createQuery(query, Object.class);
        q.getParameterValue("z");
    }

    @Test
    public void isBoundIndicatesWhetherVariableHasBeenBound() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = createQuery(query, Object.class);
        final Parameter<?> p = q.getParameter("z");
        assertFalse(q.isBound(p));
        q.setParameter(p.getName(), "Test");
        assertTrue(q.isBound(p));
        assertEquals("Test", q.getParameterValue(p.getName()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void setMaxResultsToLessThanZeroThrowsIllegalArgument() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = createQuery(query, Object.class);
        q.setMaxResults(-1);
    }

    @Test
    public void addingContextsPassesThemToQueryEvaluation() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query<?> q = createQuery(query, Object.class);
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
        final Query<?> q = createQuery(query, Object.class);
        q.getSingleResult();
    }

    @Test
    public void setPositionalParameterSetsValueAtCorrectPosition() throws Exception {
        final String query = "SELECT ?x ?z WHERE { ?x $1 ?z . }";
        final Query<?> q = createQuery(query, Object.class);
        final URI paramValue = URI.create("http://krizik.felk.cvut.cz/jopa#property");
        q.setParameter(1, paramValue);
        q.getResultList();
        verify(statementMock).executeQuery(query.replace("$1", "<" + paramValue.toString() + ">"));
    }

    @Test
    public void setPositionalParameterWithLanguageTag() throws Exception {
        final String query = "SELECT ?x WHERE { ?x rdfs:label $ . }";
        final Query<?> q = createQuery(query, Object.class);
        final String value = "Hooray";
        q.setParameter(1, value, "en");
        assertEquals(value, q.getParameterValue(1));
        final Parameter<?> p = q.getParameter(1);
        assertEquals(value, q.getParameterValue(p));
        q.getResultList();
        verify(statementMock).executeQuery(query.replace("$", "\"Hooray\"@en"));
    }
}
