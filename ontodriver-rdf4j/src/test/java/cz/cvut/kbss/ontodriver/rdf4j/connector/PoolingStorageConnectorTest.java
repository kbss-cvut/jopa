/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.query.QuerySpecification;
import cz.cvut.kbss.ontodriver.util.Transaction;
import cz.cvut.kbss.ontodriver.util.TransactionState;
import org.eclipse.rdf4j.common.iteration.CloseableIteration;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.query.BooleanQuery;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.RepositoryResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.reflect.Whitebox;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.locks.Lock;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@PrepareForTest(PoolingStorageConnector.class)
public class PoolingStorageConnectorTest {

    @Mock
    private StorageConnector centralMock;
    @Mock
    private Lock readLock;
    @Mock
    private Lock writeLock;

    private ValueFactory vf;

    private Transaction transaction;

    private PoolingStorageConnector connector;

    @BeforeEach
    public void setUp() throws Exception {
        this.vf = SimpleValueFactory.getInstance();
        this.connector = new PoolingStorageConnector(centralMock);
        final Field transactionField = AbstractConnector.class.getDeclaredField("transaction");
        transactionField.setAccessible(true);
        this.transaction = (Transaction) transactionField.get(connector);
        Whitebox.setInternalState(PoolingStorageConnector.class, "READ", readLock);
        Whitebox.setInternalState(PoolingStorageConnector.class, "WRITE", writeLock);
    }

    @Test
    public void testBegin() throws Exception {
        assertFalse(transaction.isActive());
        connector.begin();
        assertTrue(transaction.isActive());
    }

    @Test
    public void executeSelectOutsideTransactionRunsOnCentralConnector() throws Exception {
        final String query = "Some query";
        connector.executeSelectQuery(QuerySpecification.query(query));

        InOrder inOrder = inOrder(readLock, centralMock);
        inOrder.verify(readLock).lock();
        inOrder.verify(centralMock).executeSelectQuery(QuerySpecification.query(query));
        inOrder.verify(readLock).unlock();
    }

    @Test
    public void executeSelectInTransactionRunsOnTransactionalRepositoryConnection() throws Exception {
        final String query = "SELECT Some query";
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        final TupleQuery tq = mock(TupleQuery.class);
        when(conn.prepareTupleQuery(QueryLanguage.SPARQL, query)).thenReturn(tq);
        when(centralMock.acquireConnection()).thenReturn(conn);
        connector.begin();
        connector.executeSelectQuery(QuerySpecification.query(query));

        verify(conn).prepareTupleQuery(QueryLanguage.SPARQL, query);
        verify(tq).evaluate();
    }

    @Test
    public void testUnlockWhenExecuteQueryThrowsException() throws Exception {
        final String query = "Some query";
        when(centralMock.executeSelectQuery(any())).thenThrow(new Rdf4jDriverException());
        try {
            assertThrows(Rdf4jDriverException.class, () -> connector.executeSelectQuery(QuerySpecification.query(query)));
        } finally {
            verify(readLock).lock();
            verify(readLock).unlock();
        }
    }

    @Test
    public void executeBooleanQueryRunsOnCentralConnectionWhenNoTransactionIsActive() throws Exception {
        final String query = "ASK some query";
        connector.executeBooleanQuery(QuerySpecification.query(query));

        InOrder inOrder = inOrder(readLock, centralMock);
        inOrder.verify(readLock).lock();
        inOrder.verify(centralMock).executeBooleanQuery(QuerySpecification.query(query));
        inOrder.verify(readLock).unlock();
    }

    @Test
    public void executeBooleanRunsOnTransactionalConnectionWhenTransactionIsActive() throws Exception {
        final String query = "ASK some query";
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        final BooleanQuery bq = mock(BooleanQuery.class);
        when(conn.prepareBooleanQuery(QueryLanguage.SPARQL, query)).thenReturn(bq);
        when(centralMock.acquireConnection()).thenReturn(conn);
        connector.begin();
        connector.executeBooleanQuery(QuerySpecification.query(query));

        verify(conn).prepareBooleanQuery(QueryLanguage.SPARQL, query);
        verify(bq).evaluate();
    }

    @Test
    public void unlocksReadLockWhenExecuteBooleanQueryThrowsException() throws Exception {
        final String query = "ASK some query";
        when(centralMock.executeBooleanQuery(any())).thenThrow(new Rdf4jDriverException());

        try {
            assertThrows(Rdf4jDriverException.class, () -> connector.executeBooleanQuery(QuerySpecification.query(query)));
        } finally {
            verify(readLock).unlock();
        }
    }

    @Test
    public void testExecuteUpdate() throws Exception {
        connector.begin();
        final String query = "Some query";
        connector.executeUpdate(QuerySpecification.query(query));

        InOrder inOrder = inOrder(writeLock, centralMock);
        inOrder.verify(writeLock).lock();
        inOrder.verify(centralMock).executeUpdate(QuerySpecification.query(query));
        inOrder.verify(writeLock).unlock();
    }

    @Test
    public void testUnlockWhenExecuteUpdateThrowsException() throws Exception {
        connector.begin();
        final String query = "Some query";
        doThrow(new Rdf4jDriverException()).when(centralMock).executeUpdate(any());
        try {
            assertThrows(Rdf4jDriverException.class, () -> connector.executeUpdate(QuerySpecification.query(query)));
        } finally {
            verify(writeLock).unlock();
        }
    }

    @Test
    public void testGetContexts() throws Exception {
        connector.getContexts();
        verify(readLock).lock();
        verify(centralMock).getContexts();
        verify(readLock).unlock();
    }

    @Test
    public void testCommit() throws Exception {
        connector.begin();
        connector.commit();
        verify(writeLock).lock();
        verify(centralMock).begin();
        verify(centralMock).addStatements(anyCollection());
        verify(centralMock).removeStatements(anyCollection());
        verify(centralMock).commit();
        verify(writeLock).unlock();
        assertFalse(transaction.isActive());
    }

    @Test
    public void testUnlockWhenCommitThrowsException() throws Exception {
        doThrow(new Rdf4jDriverException()).when(centralMock).commit();
        connector.begin();
        try {
            assertThrows(Rdf4jDriverException.class, () -> connector.commit());
        } finally {
            verify(centralMock).begin();
            verify(centralMock).addStatements(anyCollection());
            verify(centralMock).removeStatements(anyCollection());
            verify(centralMock).commit();
            verify(writeLock).unlock();
            assertEquals(TransactionState.ABORTED, transaction.getState());
        }
    }

    @Test
    public void testRollback() throws Exception {
        connector.begin();
        connector.rollback();
        assertEquals(TransactionState.ABORTED, transaction.getState());
    }

    @Test
    public void testAddStatementsInactiveTransaction() {
        final List<Statement> statements = getStatements();
        assertThrows(IllegalStateException.class, () -> connector.addStatements(statements));
    }

    private List<Statement> getStatements() {
        final Statement stmt = mock(Statement.class);
        return Collections.singletonList(stmt);
    }

    @Test
    public void testRemoveStatementsInactiveTransaction() {
        final List<Statement> statements = getStatements();
        assertThrows(IllegalStateException.class, () -> connector.removeStatements(statements));
    }

    @Test
    public void testClose() throws Exception {
        assertTrue(connector.isOpen());
        connector.close();
        assertFalse(connector.isOpen());
    }

    @Test
    public void unwrapReturnsItselfWhenClassMatches() throws Exception {
        assertSame(connector, connector.unwrap(PoolingStorageConnector.class));
    }

    @Test
    public void transactionBeginAcquiresRepositoryConnection() throws Exception {
        connector.begin();
        verify(centralMock).acquireConnection();
    }

    @Test
    public void transactionCommitReleasesRepositoryConnection() throws Exception {
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(centralMock.acquireConnection()).thenReturn(conn);
        connector.begin();
        connector.commit();
        InOrder order = inOrder(centralMock);
        order.verify(centralMock).acquireConnection();
        order.verify(centralMock).releaseConnection(conn);
    }

    @Test
    public void transactionRollbackReleasesRepositoryConnection() throws Exception {
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(centralMock.acquireConnection()).thenReturn(conn);
        connector.begin();
        connector.rollback();
        InOrder order = inOrder(centralMock);
        order.verify(centralMock).acquireConnection();
        order.verify(centralMock).releaseConnection(conn);
    }

    @Test
    public void findStatementsReusesRepositoryConnectionDuringTransaction() throws Exception {
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(conn.getStatements(any(Resource.class), any(IRI.class), any(), anyBoolean()))
                .thenReturn(new RepositoryResult<Statement>(mock(CloseableIteration.class)));
        when(centralMock.acquireConnection()).thenReturn(conn);
        final Resource res = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        connector.begin();
        connector.findStatements(res, property, null, false);
        verify(centralMock).acquireConnection();
        verify(conn).getStatements(res, property, null, false);
    }

    @Test
    public void exceptionInFindStatementsCausesTransactionRollback() throws Exception {
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(conn.getStatements(any(Resource.class), any(IRI.class), any(), anyBoolean()))
                .thenThrow(new RepositoryException());
        when(centralMock.acquireConnection()).thenReturn(conn);
        final Resource res = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        final Connector spy = spy(connector);
        doCallRealMethod().when(spy).begin();
        spy.begin();
        doCallRealMethod().when(spy).findStatements(any(Resource.class), any(IRI.class), any(), anyBoolean());
        try {
            assertThrows(Rdf4jDriverException.class, () -> spy.findStatements(res, property, null, false));
        } finally {
            verify(spy).rollback();
        }
    }

    @Test
    public void closeReleasesActiveConnection() throws Exception {
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(centralMock.acquireConnection()).thenReturn(conn);
        connector.begin();
        connector.close();
        verify(centralMock).releaseConnection(conn);
    }

    @Test
    public void repeatedCloseIsHandled() throws Exception {
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(centralMock.acquireConnection()).thenReturn(conn);
        connector.begin();
        connector.close();
        connector.close();
        verify(centralMock).releaseConnection(conn);
    }

    @Test
    public void containsReturnsTrueWhenLocalModelContainsStatement() throws Exception {
        final Resource subject = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(centralMock.acquireConnection()).thenReturn(conn);
        connector.begin();
        connector
                .addStatements(Collections.singletonList(vf.createStatement(subject, property, vf.createLiteral(117))));
        assertTrue(connector.containsStatement(subject, property, null, false, Collections.emptySet()));
        verify(conn, never()).hasStatement(subject, property, null, false);
    }

    @Test
    public void containsReturnsTrueWhenCentralConnectorsContainsStatementAndLocalDoesNot() throws Exception {
        final Resource subject = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(conn.hasStatement(subject, property, null, false)).thenReturn(true);
        when(centralMock.acquireConnection()).thenReturn(conn);
        connector.begin();
        assertTrue(connector.containsStatement(subject, property, null, false, Collections.emptySet()));
    }

    @Test
    public void containsReturnsFalseWhenStatementsWasRemovedLocally() throws Exception {
        final Resource subject = vf.createIRI(Generator.generateUri().toString());
        final IRI property = vf.createIRI(Generator.generateUri().toString());
        final RepositoryConnection conn = mock(RepositoryConnection.class);
        when(centralMock.acquireConnection()).thenReturn(conn);
        connector.begin();
        connector.removeStatements(
                Collections.singletonList(vf.createStatement(subject, property, vf.createLiteral(117))));
        assertFalse(connector.containsStatement(subject, property, null, false, Collections.emptySet()));
    }
}
