/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
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

    @BeforeEach
    public void setUp() throws Exception {
        this.statement = new OwlapiStatement(executorFactoryMock, connectionMock);
    }

    @Test
    public void executeStatementOnTransactionalUsesTransactionalSnapshot() throws Exception {
        when(executorFactoryMock.getStatementExecutor(any())).thenReturn(executorMock);
        when(executorMock.executeQuery(any(QuerySpecification.class))).thenReturn(resultSetMock);
        statement.useOntology(Statement.StatementOntology.TRANSACTIONAL);
        statement.executeQuery(QUERY);
        verify(executorFactoryMock).getStatementExecutor(Statement.StatementOntology.TRANSACTIONAL);
        final ArgumentCaptor<QuerySpecification> queryCaptor = ArgumentCaptor.forClass(QuerySpecification.class);
        verify(executorMock).executeQuery(queryCaptor.capture());
        assertEquals(QUERY, queryCaptor.getValue().getQuery());
        assertEquals(statement, queryCaptor.getValue().getStatement());
    }

    @Test
    public void executeUpdateOnCentralUsesTheLiveOntology() throws Exception {
        when(executorFactoryMock.getStatementExecutor(any())).thenReturn(executorMock);
        statement.useOntology(Statement.StatementOntology.SHARED);
        statement.executeUpdate(UPDATE);
        verify(executorFactoryMock).getStatementExecutor(Statement.StatementOntology.SHARED);
        verify(executorMock).executeUpdate(QuerySpecification.query(UPDATE));
    }

    @Test
    public void executeUpdateCommitsConnectionIfSetToAutoCommit() throws Exception {
        when(executorFactoryMock.getStatementExecutor(any())).thenReturn(executorMock);
        statement.executeUpdate(UPDATE);
        verify(connectionMock).commitIfAuto();
    }

    @Test
    public void executeQueryOnClosedThrowsException() throws Exception {
        statement.close();
        assertThrows(IllegalStateException.class, () -> statement.executeQuery(QUERY));
    }

    @Test
    public void executeUpdateOnClosedThrowsException() throws Exception {
        statement.close();
        assertThrows(IllegalStateException.class, () -> statement.executeUpdate(UPDATE));
    }

    @Test
    public void closeClosesExistingResultSet() throws Exception {
        when(executorFactoryMock.getStatementExecutor(any())).thenReturn(executorMock);
        when(executorMock.executeQuery(any(QuerySpecification.class))).thenReturn(resultSetMock);
        statement.executeQuery(QUERY);
        statement.close();
        verify(resultSetMock).close();
        assertFalse(statement.isOpen());
    }

    @Test
    public void executeQueryClosesCurrentResultSet() throws Exception {
        when(executorFactoryMock.getStatementExecutor(any())).thenReturn(executorMock);
        when(executorMock.executeQuery(any(QuerySpecification.class))).thenReturn(resultSetMock);
        statement.executeQuery(QUERY);
        statement.executeQuery(QUERY);
        verify(resultSetMock).close();
    }

    @Test
    public void executeUpdateClosesCurrentResultSet() throws Exception {
        when(executorFactoryMock.getStatementExecutor(any())).thenReturn(executorMock);
        when(executorMock.executeQuery(any(QuerySpecification.class))).thenReturn(resultSetMock);
        statement.executeQuery(QUERY);
        statement.executeUpdate(UPDATE);
        verify(resultSetMock).close();
    }
}
