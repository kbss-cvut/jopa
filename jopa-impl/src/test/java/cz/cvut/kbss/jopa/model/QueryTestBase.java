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

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryFactory;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.Procedure;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

public abstract class QueryTestBase {

    static final String UPDATE_QUERY = "INSERT DATA { ?inst a ?type . }";

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    ConnectionWrapper connectionWrapperMock;
    @Mock
    UnitOfWorkImpl uowMock;
    @Mock
    Statement statementMock;
    @Mock
    ResultSet resultSetMock;

    @Mock
    Procedure handler;

    SparqlQueryFactory queryFactory;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(connectionWrapperMock.createStatement()).thenReturn(statementMock);
        when(statementMock.executeQuery(anyString())).thenReturn(resultSetMock);
        doAnswer((invocationOnMock) -> {
            resultSetMock.close();
            return null;
        }).when(statementMock).close();
        this.queryFactory = new SparqlQueryFactory(uowMock, connectionWrapperMock);
    }

    abstract Query createQuery(String query, Class<?> resultType);

    @Test
    public void getResultListWithoutParameterSettingJustPassesTheOriginalQuery() throws Exception {
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        q.getResultList();
        verify(connectionWrapperMock).createStatement();
        verify(statementMock).executeQuery(query);
    }

    @Test
    public void setParameterByNameSetsAllOccurrencesOfVariableInQuery() throws Exception {
        final String query = "SELECT ?y ?z WHERE { ?x ?y ?z . ?z ?y ?x . }";
        final Query q = createQuery(query, Object.class);
        q.setParameter("x", "Individual");
        q.getResultList();
        verify(connectionWrapperMock).createStatement();
        verify(statementMock).executeQuery("SELECT ?y ?z WHERE { \"Individual\" ?y ?z . ?z ?y \"Individual\" . }");
    }

    @Test
    public void setParameterByParameterSetsValueWithCorrectType() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        final Parameter<URI> p = (Parameter<URI>) q.getParameter("z");
        q.setParameter(p, URI.create("http://krizik.felk.cvut.cz"));
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y <http://krizik.felk.cvut.cz> .}");
    }

    @Test
    public void testSetStringParameterWithLanguageTag() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        q.setParameter("z", "Object", "en");
        assertEquals("Object", q.getParameterValue("z"));
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y \"Object\"@en .}");
    }

    @Test
    public void testSetStringParameterWithLanguageTagUsingParameterMethod() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        final Parameter<String> p = (Parameter<String>) q.getParameter("z");
        q.setParameter(p, "Object", "cs");
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y \"Object\"@cs .}");
    }

    @Test(expected = IllegalStateException.class)
    public void getParameterValueThrowsIllegalStateForUnboundParam() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        q.getParameterValue("z");
    }

    @Test
    public void isBoundIndicatesWhetherVariableHasBeenBound() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        final Parameter<?> p = q.getParameter("z");
        assertFalse(q.isBound(p));
        q.setParameter(p.getName(), "Test");
        assertTrue(q.isBound(p));
        assertEquals("Test", q.getParameterValue(p.getName()));
    }

    @Test(expected = IllegalArgumentException.class)
    public void setMaxResultsToLessThanZeroThrowsIllegalArgument() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        q.setMaxResults(-1);
    }

    @Test(expected = NoResultException.class)
    public void getSingleResultWithoutResultThrowsNoResultException() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        q.getSingleResult();
    }

    @Test
    public void setPositionalParameterSetsValueAtCorrectPosition() throws Exception {
        final String query = "SELECT ?x ?z WHERE { ?x $1 ?z . }";
        final Query q = createQuery(query, Object.class);
        final URI paramValue = URI.create("http://krizik.felk.cvut.cz/jopa#property");
        q.setParameter(1, paramValue);
        q.getResultList();
        verify(statementMock).executeQuery(query.replace("$1", "<" + paramValue.toString() + ">"));
    }

    @Test
    public void setPositionalParameterWithLanguageTag() throws Exception {
        final String query = "SELECT ?x WHERE { ?x rdfs:label $ . }";
        final Query q = createQuery(query, Object.class);
        final String value = "Hooray";
        q.setParameter(1, value, "en");
        assertEquals(value, q.getParameterValue(1));
        final Parameter<?> p = q.getParameter(1);
        assertEquals(value, q.getParameterValue(p));
        q.getResultList();
        verify(statementMock).executeQuery(query.replace("$", "\"Hooray\"@en"));
    }

    @Test
    public void closesStatementAndResultSetUponSelectFinish() throws Exception {
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        q.getResultList();
        verify(statementMock).close();
        verify(resultSetMock).close();
    }

    @Test
    public void closesStatementUponUpdateFinish() throws Exception {
        final String query = "INSERT DATA { ?x ?y ?z .}";
        final Query q = createQuery(query, Void.class);
        q.executeUpdate();
        verify(statementMock).close();
    }
}
