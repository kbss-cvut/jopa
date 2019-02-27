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
package cz.cvut.kbss.ontodriver.owlapi.query;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiConnection;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.Assert.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class OwlapiStatementTest {

    private static final String QUERY = "SELECT ?x WHERE { ?x ?y ?z . }";
    private static final String UPDATE = "INSERT DATA { a b c . }";

    @Mock
    private TransactionalStatementExecutor executorMock;
    @Mock
    private StatementExecutorFactory executorFactoryMock;
    @Mock
    private OwlapiConnection connectionMock;
    @Mock
    private ResultSet resultSetMock;

    private OwlapiStatement statement;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(executorFactoryMock.getStatementExecutor(any())).thenReturn(executorMock);
        this.statement = new OwlapiStatement(executorFactoryMock, connectionMock);
        when(executorMock.executeQuery(anyString(), eq(statement))).thenReturn(resultSetMock);
    }

    @Test
    public void executeStatementOnTransactionalUsesTransactionalSnapshot() throws Exception {
        statement.useOntology(Statement.StatementOntology.TRANSACTIONAL);
        statement.executeQuery(QUERY);
        verify(executorFactoryMock).getStatementExecutor(Statement.StatementOntology.TRANSACTIONAL);
        verify(executorMock).executeQuery(QUERY, statement);
    }

    @Test
    public void executeUpdateOnCentralUsesTheLiveOntology() throws Exception {
        statement.useOntology(Statement.StatementOntology.CENTRAL);
        statement.executeUpdate(UPDATE);
        verify(executorFactoryMock).getStatementExecutor(Statement.StatementOntology.CENTRAL);
        verify(executorMock).executeUpdate(UPDATE);
    }

    @Test
    public void executeUpdateCommitsConnectionIfSetToAutoCommit() throws Exception {
        statement.executeUpdate(UPDATE);
        verify(connectionMock).commitIfAuto();
    }

    @Test(expected = IllegalStateException.class)
    public void executeQueryOnClosedThrowsException() throws Exception {
        statement.close();
        statement.executeQuery(QUERY);
    }

    @Test(expected = IllegalStateException.class)
    public void executeUpdateOnClosedThrowsException() throws Exception {
        statement.close();
        statement.executeUpdate(UPDATE);
    }

    @Test
    public void closeClosesExistingResultSet() throws Exception {
        statement.executeQuery(QUERY);
        statement.close();
        verify(resultSetMock).close();
        assertFalse(statement.isOpen());
    }

    @Test
    public void executeQueryClosesCurrentResultSet() throws Exception {
        statement.executeQuery(QUERY);
        statement.executeQuery(QUERY);
        verify(resultSetMock).close();
    }

    @Test
    public void executeUpdateClosesCurrentResultSet() throws Exception {
        statement.executeQuery(QUERY);
        statement.executeUpdate(UPDATE);
        verify(resultSetMock).close();
    }
}