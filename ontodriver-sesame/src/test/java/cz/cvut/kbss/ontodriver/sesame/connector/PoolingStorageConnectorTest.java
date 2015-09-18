package cz.cvut.kbss.ontodriver.sesame.connector;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.locks.Lock;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;

import cz.cvut.kbss.ontodriver.sesame.TestUtils;
import cz.cvut.kbss.ontodriver.sesame.Transaction;
import cz.cvut.kbss.ontodriver.sesame.TransactionState;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

public class PoolingStorageConnectorTest {

    @Mock
    private Connector centralMock;
    @Mock
    private Lock readLock;
    @Mock
    private Lock writeLock;

    private Transaction transaction;

    private PoolingStorageConnector connector;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.connector = new PoolingStorageConnector(centralMock);
        final Field transactionField = AbstractConnector.class.getDeclaredField("transaction");
        transactionField.setAccessible(true);
        this.transaction = (Transaction) transactionField.get(connector);
        TestUtils.setMock("READ", PoolingStorageConnector.class, readLock);
        TestUtils.setMock("WRITE", PoolingStorageConnector.class, writeLock);
    }

    @Test
    public void testBegin() throws Exception {
        assertFalse(transaction.isActive());
        connector.begin();
        assertTrue(transaction.isActive());
    }

    @Test
    public void testExecuteQuery() throws Exception {
        connector.begin();
        final String query = "Some query";
        connector.executeSelectQuery(query);

        InOrder inOrder = inOrder(readLock, centralMock);
        inOrder.verify(readLock).lock();
        inOrder.verify(centralMock).executeSelectQuery(query);
        inOrder.verify(readLock).unlock();
    }

    @Test(expected = SesameDriverException.class)
    public void testUnlockWhenExecuteQueryThrowsException() throws Exception {
        connector.begin();
        final String query = "Some query";
        when(centralMock.executeSelectQuery(query)).thenThrow(new SesameDriverException());
        try {
            connector.executeSelectQuery(query);
        } finally {
            verify(readLock).lock();
            verify(readLock).unlock();
        }
    }

    @Test
    public void testExecuteBooleanQuery() throws Exception {
        connector.begin();
        final String query = "ASK some query";
        connector.executeBooleanQuery(query);

        InOrder inOrder = inOrder(readLock, centralMock);
        inOrder.verify(readLock).lock();
        inOrder.verify(centralMock).executeBooleanQuery(query);
        inOrder.verify(readLock).unlock();
    }

    @Test(expected = SesameDriverException.class)
    public void unlocksReadLockWhenExecuteBooleanQueryThrowsException() throws Exception {
        connector.begin();
        final String query = "ASK some query";
        when(centralMock.executeBooleanQuery(query)).thenThrow(new SesameDriverException());

        try {
            connector.executeBooleanQuery(query);
        } finally {
            verify(readLock).unlock();
        }
    }

    @Test
    public void testExecuteUpdate() throws Exception {
        connector.begin();
        final String query = "Some query";
        connector.executeUpdate(query);

        InOrder inOrder = inOrder(writeLock, centralMock);
        inOrder.verify(writeLock).lock();
        inOrder.verify(centralMock).executeUpdate(query);
        inOrder.verify(writeLock).unlock();
    }

    @Test(expected = SesameDriverException.class)
    public void testUnlockWhenExecuteUpdateThrowsException() throws Exception {
        connector.begin();
        final String query = "Some query";
        doThrow(new SesameDriverException()).when(centralMock).executeUpdate(query);
        try {
            connector.executeUpdate(query);
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
        verify(centralMock).addStatements(any(Collection.class));
        verify(centralMock).removeStatements(any(Collection.class));
        verify(centralMock).commit();
        verify(writeLock).unlock();
        assertFalse(transaction.isActive());
    }

    @Test(expected = SesameDriverException.class)
    public void testUnlockWhenCommitThrowsException() throws Exception {
        doThrow(new SesameDriverException()).when(centralMock).commit();
        connector.begin();
        try {
            connector.commit();
        } finally {
            verify(centralMock).begin();
            verify(centralMock).addStatements(any(Collection.class));
            verify(centralMock).removeStatements(any(Collection.class));
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

    @Test(expected = IllegalStateException.class)
    public void testAddStatementsInactiveTransaction() {
        final List<Statement> statements = getStatements();
        connector.addStatements(statements);
    }

    private List<Statement> getStatements() {
        final Statement stmt = mock(Statement.class);
        return Collections.singletonList(stmt);
    }

    @Test(expected = IllegalStateException.class)
    public void testRemoveStatementsInactiveTransaction() throws Exception {
        final List<Statement> statements = getStatements();
        connector.removeStatements(statements);
    }

    @Test
    public void testFindStatements() throws Exception {
        final Resource subject = mock(Resource.class);
        final URI property = mock(URI.class);
        connector.begin();
        connector.findStatements(subject, property, null, false, (URI[]) null);
        verify(readLock).lock();
        verify(centralMock).findStatements(subject, property, null, false, (URI[]) null);
        verify(readLock).unlock();
    }

    @Test
    public void testClose() throws Exception {
        assertTrue(connector.isOpen());
        connector.close();
        assertFalse(connector.isOpen());
    }

}
