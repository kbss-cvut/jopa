/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.jena.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.Query;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class JenaStatementTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private StatementExecutor executor;

    private JenaStatement statement;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.statement = new JenaStatement(executor);
    }

    @Test
    public void executeQueryExecutesSelect() throws JenaDriverException {
        final SelectResultSet rsMock = mock(SelectResultSet.class);
        when(executor.executeSelectQuery(any(), any())).thenReturn(rsMock);
        final String query = "SELECT * WHERE { ?x ?y ?z .}";
        final ResultSet rs = statement.executeQuery(query);
        verify(executor).executeSelectQuery(any(Query.class), eq(Statement.StatementOntology.CENTRAL));
        assertSame(rsMock, rs);
    }

    @Test
    public void executeQueryExecutesAsk() throws JenaDriverException {
        final AskResultSet rsMock = mock(AskResultSet.class);
        when(executor.executeAskQuery(any(), any())).thenReturn(rsMock);
        final String query = "ASK { ?x a <http://xmlns.com/foaf/0.1/Person> . }";
        final ResultSet rs = statement.executeQuery(query);
        verify(executor).executeAskQuery(any(Query.class), eq(Statement.StatementOntology.CENTRAL));
        assertSame(rsMock, rs);
    }

    @Test
    public void executeQueryExecutesAskWhenPrefixesAreDeclared() throws JenaDriverException {
        final AskResultSet rsMock = mock(AskResultSet.class);
        when(executor.executeAskQuery(any(), any())).thenReturn(rsMock);
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/> \n" +
                "ASK { ?x a foaf:Person . }";
        statement.executeQuery(query);
        verify(executor).executeAskQuery(any(Query.class), eq(Statement.StatementOntology.CENTRAL));
    }

    @Test
    public void executeQuerySetsStatementOnResultSet() throws JenaDriverException {
        final SelectResultSet rsMock = mock(SelectResultSet.class);
        when(executor.executeSelectQuery(any(), any())).thenReturn(rsMock);
        final String query = "SELECT * WHERE { ?x ?y ?z .}";
        final AbstractResultSet rs = (AbstractResultSet) statement.executeQuery(query);
        verify(rs).setStatement(statement);
    }

    @Test
    public void executeQueryThrowsIllegalStateExceptionForClosedStatement() throws JenaDriverException {
        thrown.expect(IllegalStateException.class);
        thrown.expectMessage("Statement is closed.");
        statement.close();
        statement.executeQuery("SELECT * WHERE { ?x ?y ?z .}");
    }

    @Test
    public void executeQueryThrowsJenaDriverExceptionForMalformedQuery() throws JenaDriverException {
        final String query = "SELECT * WHERE { ?x ?y df .}";
        thrown.expect(JenaDriverException.class);
        thrown.expectMessage(containsString("Unable to parse query "));
        statement.executeQuery(query);
    }

    @Test
    public void executeQueryClosesCurrentResultSet() throws JenaDriverException {
        final SelectResultSet rsMock = mock(SelectResultSet.class);
        when(executor.executeSelectQuery(any(), any())).thenReturn(rsMock);
        final String query = "SELECT * WHERE { ?x ?y ?z .}";
        final AbstractResultSet rs = (AbstractResultSet) statement.executeQuery(query);
        assertSame(rsMock, rs);
        statement.executeQuery(query);
        verify(rsMock).close();
    }

    @Test
    public void executeUpdateExecutesUpdateQuery() throws JenaDriverException {
        final String query = "INSERT DATA { _:b1 a <http://xmlns.com/foaf/0.1/Person> . }";
        statement.executeUpdate(query);
        verify(executor).executeUpdate(query, Statement.StatementOntology.CENTRAL);
    }

    @Test
    public void executeUpdateThrowsIllegalStateExceptionForClosedStatement() throws JenaDriverException {
        thrown.expect(IllegalStateException.class);
        thrown.expectMessage("Statement is closed.");
        statement.close();
        statement.executeUpdate("INSERT DATA { _:b1 a <http://xmlns.com/foaf/0.1/Person> . }");
    }

    @Test
    public void executeUpdateClosesExistingResultSet() throws JenaDriverException {
        final AskResultSet rsMock = mock(AskResultSet.class);
        when(executor.executeAskQuery(any(), any())).thenReturn(rsMock);
        final String query = "ASK { ?x a <http://xmlns.com/foaf/0.1/Person> . }";
        statement.executeQuery(query);
        final String update = "INSERT DATA { _:b1 a <http://xmlns.com/foaf/0.1/Person> . }";
        statement.executeUpdate(update);
        verify(rsMock).close();
    }

    @Test
    public void closeClosesCurrentResultSetAsWell() throws Exception {
        final AskResultSet rsMock = mock(AskResultSet.class);
        when(executor.executeAskQuery(any(), any())).thenReturn(rsMock);
        final String query = "ASK { ?x a <http://xmlns.com/foaf/0.1/Person> . }";
        statement.executeQuery(query);
        statement.close();
        verify(rsMock).close();
        assertFalse(statement.isOpen());
    }

    @Test
    public void settingTargetOntologyIsReflectedInQueryExecutionParameter() throws Exception {
        final SelectResultSet rsMock = mock(SelectResultSet.class);
        when(executor.executeSelectQuery(any(), any())).thenReturn(rsMock);
        final String query = "SELECT * WHERE { ?x ?y ?z .}";
        statement.useOntology(Statement.StatementOntology.TRANSACTIONAL);
        assertEquals(Statement.StatementOntology.TRANSACTIONAL, statement.getStatementOntology());
        final ResultSet rs = statement.executeQuery(query);
        verify(executor).executeSelectQuery(any(Query.class), eq(Statement.StatementOntology.TRANSACTIONAL));
        assertSame(rsMock, rs);
    }
}