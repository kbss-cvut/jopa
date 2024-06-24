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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.model.query.Parameter;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryFactory;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.utils.Procedure;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.iteration.ResultSetIterator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import java.net.URI;
import java.util.List;
import java.util.Optional;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

abstract class QueryTestBase {

    static final String SELECT_QUERY =
            "SELECT ?x WHERE { ?x a <http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA> . }";
    static final String UPDATE_QUERY = "INSERT DATA { ?inst a ?type . }";

    @Mock
    ConnectionWrapper connectionWrapperMock;
    @Mock
    UnitOfWork uowMock;
    @Mock
    Statement statementMock;
    @Mock
    ResultSet resultSetMock;
    @Mock
    ResultRow resultRow;
    @Mock
    ResultSetIterator resultSetIterator;

    @Mock
    Procedure handler;

    @Mock
    Procedure ensureOpenProcedure;

    SparqlQueryFactory queryFactory;

    @BeforeEach
    void setUp() throws Exception {
        when(connectionWrapperMock.createStatement()).thenReturn(statementMock);
        when(statementMock.executeQuery(any())).thenReturn(resultSetMock);
        doAnswer((invocationOnMock) -> {
            resultSetMock.close();
            return null;
        }).when(statementMock).close();
        when(resultSetMock.iterator()).thenReturn(resultSetIterator);
        when(resultSetIterator.next()).thenReturn(resultRow);
        when(resultSetMock.stream()).thenCallRealMethod();
        when(resultSetMock.spliterator()).thenCallRealMethod();
        this.queryFactory = new SparqlQueryFactory(uowMock, connectionWrapperMock);
    }

    abstract AbstractQuery createQuery(String query, Class<?> resultType);

    abstract AbstractQuery createQuery(String query);

    @Test
    void getResultListWithoutParameterSettingJustPassesTheOriginalQuery() throws Exception {
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        q.getResultList();
        verify(connectionWrapperMock).createStatement();
        verify(statementMock).executeQuery(query);
    }

    @Test
    void setParameterByNameSetsAllOccurrencesOfVariableInQuery() throws Exception {
        final String query = "SELECT ?y ?z WHERE { ?x ?y ?z . ?z ?y ?x . }";
        final Query q = createQuery(query, Object.class);
        q.setParameter("x", "Individual");
        q.getResultList();
        verify(connectionWrapperMock).createStatement();
        verify(statementMock).executeQuery("SELECT ?y ?z WHERE { \"Individual\" ?y ?z . ?z ?y \"Individual\" . }");
    }

    @Test
    void setParameterByParameterSetsValueWithCorrectType() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        final Parameter<URI> p = (Parameter<URI>) q.getParameter("z");
        q.setParameter(p, URI.create("http://krizik.felk.cvut.cz"));
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y <http://krizik.felk.cvut.cz> .}");
    }

    @Test
    void testSetStringParameterWithLanguageTag() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        q.setParameter("z", "Object", "en");
        assertEquals("Object", q.getParameterValue("z"));
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y \"Object\"@en .}");
    }

    @Test
    void testSetStringParameterWithLanguageTagUsingParameterMethod() throws Exception {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        final Parameter<String> p = (Parameter<String>) q.getParameter("z");
        q.setParameter(p, "Object", "cs");
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x ?y WHERE { ?x ?y \"Object\"@cs .}");
    }

    @Test
    void getParameterValueThrowsIllegalStateForUnboundParam() {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        assertThrows(IllegalStateException.class, () -> q.getParameterValue("z"));
    }

    @Test
    void isBoundIndicatesWhetherVariableHasBeenBound() {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        final Parameter<?> p = q.getParameter("z");
        assertFalse(q.isBound(p));
        q.setParameter(p.getName(), "Test");
        assertTrue(q.isBound(p));
        assertEquals("Test", q.getParameterValue(p.getName()));
    }

    @Test
    void setMaxResultsToLessThanZeroThrowsIllegalArgument() {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        assertThrows(IllegalArgumentException.class, () -> q.setMaxResults(-1));
    }

    @Test
    void getSingleResultWithoutResultThrowsNoResultException() {
        final String query = "SELECT ?x ?y WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        assertThrows(NoResultException.class, q::getSingleResult);
    }

    @Test
    void setPositionalParameterSetsValueAtCorrectPosition() throws Exception {
        final String query = "SELECT ?x ?z WHERE { ?x $1 ?z . }";
        final Query q = createQuery(query, Object.class);
        final URI paramValue = URI.create("http://krizik.felk.cvut.cz/jopa#property");
        q.setParameter(1, paramValue);
        q.getResultList();
        verify(statementMock).executeQuery(query.replace("$1", "<" + paramValue + ">"));
    }

    @Test
    void setPositionalParameterWithLanguageTag() throws Exception {
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
    void closesStatementAndResultSetUponSelectFinish() throws Exception {
        final String query = "SELECT ?x ?y ?z WHERE { ?x ?y ?z .}";
        final Query q = createQuery(query, Object.class);
        q.getResultList();
        verify(statementMock).close();
        verify(resultSetMock).close();
    }

    @Test
    void closesStatementUponUpdateFinish() throws Exception {
        final String query = "INSERT DATA { ?x ?y ?z .}";
        final Query q = createQuery(query, Void.class);
        q.executeUpdate();
        verify(statementMock).close();
    }

    @Test
    void setUntypedParameterByPositionAddsValueDirectlyIntoQueryString() throws Exception {
        final String query = "SELECT * WHERE { ?x ?y ?z . } LIMIT $1";
        final Integer value = 15;
        final Query q = createQuery(query, Object.class);
        q.setUntypedParameter(1, value);
        q.getResultList();
        verify(statementMock).executeQuery(query.replace("$1", value.toString()));
    }

    @Test
    void setUntypedParameterByNameAddsValueDirectlyIntoQueryString() throws Exception {
        final String query = "SELECT * WHERE { ?x ?y ?z . } OFFSET ?offset";
        final Integer value = 15;
        final Query q = createQuery(query, Object.class);
        q.setUntypedParameter("offset", value);
        q.getResultList();
        verify(statementMock).executeQuery(query.replace("?offset", value.toString()));
    }

    @Test
    void setUntypedParameterAddsValueDirectlyIntoQueryString() throws Exception {
        final String query = "SELECT * WHERE { ?x ?y ?z . } OFFSET ?offset";
        final Integer value = 15;
        final Query q = createQuery(query, Object.class);
        final Parameter<Integer> param = (Parameter<Integer>) q.getParameter("offset");
        q.setUntypedParameter(param, value);
        q.getResultList();
        verify(statementMock).executeQuery(query.replace("?offset", value.toString()));
    }

    @Test
    void settingFirstResultToLargerThanResultCountReturnsEmptyList() throws Exception {
        final Query q = createQuery(SELECT_QUERY, OWLClassA.class);
        when(resultSetMock.getColumnCount()).thenReturn(1);
        // Three results
        when(resultSetMock.hasNext()).thenReturn(true).thenReturn(true).thenReturn(true).thenReturn(false);
        q.setFirstResult(3);
        final List result = q.getResultList();
        assertTrue(result.isEmpty());
        verify(resultSetMock, never()).getObject(any());
    }

    @Test
    void settingFirstResultToNegativeThrowsIllegalArgumentException() throws Exception {
        final Query q = createQuery(SELECT_QUERY, OWLClassA.class);
        final IllegalArgumentException result =
                assertThrows(IllegalArgumentException.class, () -> q.setFirstResult(-1));
        assertThat(result.getMessage(), containsString("offset to less than 0"));
        verify(resultSetMock, never()).hasNext();
    }

    @Test
    void executeUpdateEnsuresPersistenceContextIsOpen() {
        final AbstractQuery q = createQuery(UPDATE_QUERY);
        q.executeUpdate();
        verify(ensureOpenProcedure).execute();
    }

    @Test
    void getResultListEnsuresPersistenceContextIsOpen() {
        final AbstractQuery q = createQuery(SELECT_QUERY, OWLClassA.class);
        q.getResultList();
        verify(ensureOpenProcedure).execute();
    }

    @Test
    void setParameterSupportsUsingEntityAsParameterValue() throws Exception {
        final MetamodelImpl mm = mock(MetamodelImpl.class);
        final MetamodelMocks metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(mm);
        when(uowMock.getMetamodel()).thenReturn(mm);
        when(uowMock.isEntityType(any())).thenReturn(true);
        final OWLClassA a = Generators.generateOwlClassAInstance();
        final AbstractQuery q = createQuery("SELECT ?x WHERE { ?x ?hasA ?a . }", OWLClassD.class);
        q.setParameter("a", a);
        q.getResultList();
        verify(statementMock).executeQuery("SELECT ?x WHERE { ?x ?hasA <" + a.getUri() + "> . }");
    }

    @Test
    void executeQueryAppliesQueryHints() throws Exception {
        final AbstractQuery q = createQuery(SELECT_QUERY, OWLClassA.class);
        final String hintName = "jopa.query.testHint";
        final QueryHintsHandler.Hint hint = spy(new TestHint(hintName));
        QueryHintsHandler.Hint.registerHint(hint);
        q.setHint(hintName, true);
        q.executeQuery((r) -> {});
        verify(hint).apply(true, q, statementMock);
    }

    private static class TestHint extends QueryHintsHandler.Hint {

        TestHint(String name) {
            super(name, null);
        }

        @Override
        void applyToQuery(Object hintValue, AbstractQuery query, Statement statement) {
            // Do nothing
        }
    }

    @Test
    void executeQueryForStreamAppliesQueryHints() throws Exception {
        when(resultSetMock.isOpen()).thenReturn(true);
        final AbstractQuery q = createQuery(SELECT_QUERY, OWLClassA.class);
        final String hintName = "jopa.query.testHint";
        final QueryHintsHandler.Hint hint = spy(new TestHint(hintName));
        QueryHintsHandler.Hint.registerHint(hint);
        q.setHint(hintName, false);
        q.executeQueryForStream((r) -> Optional.empty());
        verify(hint).apply(false, q, statementMock);
    }

    @Test
    void executeUpdateAppliesQueryHints() {
        final AbstractQuery q = createQuery(UPDATE_QUERY);
        final String hintName = "jopa.query.testHint";
        final QueryHintsHandler.Hint hint = spy(new TestHint(hintName));
        QueryHintsHandler.Hint.registerHint(hint);
        q.setHint(hintName, true);
        q.executeUpdate();
        verify(hint).apply(true, q, statementMock);
    }
}
