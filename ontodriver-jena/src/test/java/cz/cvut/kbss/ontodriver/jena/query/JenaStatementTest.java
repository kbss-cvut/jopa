/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class JenaStatementTest {

    @Mock
    private StatementExecutor executor;

    private JenaStatement statement;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
        this.statement = new JenaStatement(executor);
    }

    @Test
    public void executeQueryExecutesSelect() throws JenaDriverException {
        final SelectResultSet rsMock = mock(SelectResultSet.class);
        when(executor.executeSelectQuery(any(), any())).thenReturn(rsMock);
        final String query = "SELECT * WHERE { ?x ?y ?z .}";
        final ResultSet rs = statement.executeQuery(query);
        verify(executor).executeSelectQuery(any(Query.class), eq(Statement.StatementOntology.SHARED));
        assertSame(rsMock, rs);
    }

    @Test
    public void executeQueryExecutesAsk() throws JenaDriverException {
        final AskResultSet rsMock = mock(AskResultSet.class);
        when(executor.executeAskQuery(any(), any())).thenReturn(rsMock);
        final String query = "ASK { ?x a <http://xmlns.com/foaf/0.1/Person> . }";
        final ResultSet rs = statement.executeQuery(query);
        verify(executor).executeAskQuery(any(Query.class), eq(Statement.StatementOntology.SHARED));
        assertSame(rsMock, rs);
    }

    @Test
    public void executeQueryExecutesAskWhenPrefixesAreDeclared() throws JenaDriverException {
        final AskResultSet rsMock = mock(AskResultSet.class);
        when(executor.executeAskQuery(any(), any())).thenReturn(rsMock);
        final String query = "PREFIX foaf: <http://xmlns.com/foaf/0.1/> \n" +
                "ASK { ?x a foaf:Person . }";
        statement.executeQuery(query);
        verify(executor).executeAskQuery(any(Query.class), eq(Statement.StatementOntology.SHARED));
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
        statement.close();
        assertThrows(IllegalStateException.class, () -> statement.executeQuery("SELECT * WHERE { ?x ?y ?z .}"));
    }

    @Test
    public void executeQueryThrowsJenaDriverExceptionForMalformedQuery() {
        final String query = "SELECT * WHERE { ?x ?y df .}";
        final JenaDriverException ex = assertThrows(JenaDriverException.class, () -> statement.executeQuery(query));
        assertThat(ex.getMessage(), containsString("Unable to parse query "));
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
        verify(executor).executeUpdate(query, Statement.StatementOntology.SHARED);
    }

    @Test
    public void executeUpdateThrowsIllegalStateExceptionForClosedStatement() throws JenaDriverException {
        statement.close();
        assertThrows(IllegalStateException.class,
                () -> statement.executeUpdate("INSERT DATA { _:b1 a <http://xmlns.com/foaf/0.1/Person> . }"));
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
