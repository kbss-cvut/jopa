package cz.cvut.kbss.ontodriver.sesame.connector;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.locks.Lock;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.openrdf.model.Statement;

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
		connector.executeQuery(query);
		verify(readLock).lock();
		verify(centralMock).executeQuery(query);
		verify(readLock).unlock();
	}

	@Test(expected = SesameDriverException.class)
	public void testUnlockWhenExecuteQueryThrowsException() throws Exception {
		connector.begin();
		final String query = "Some query";
		when(centralMock.executeQuery(query)).thenThrow(new SesameDriverException());
		try {
			connector.executeQuery(query);
		} finally {
			verify(readLock).lock();
			verify(readLock).unlock();
		}
	}

	@Test
	public void testExecuteUpdate() throws Exception {
		connector.begin();
		final String query = "Some query";
		connector.executeUpdate(query);
		verify(writeLock).lock();
		verify(centralMock).executeUpdate(query);
		verify(writeLock).unlock();
	}

	@Test(expected = SesameDriverException.class)
	public void testUnlockWhenExecuteUpdateThrowsException() throws Exception {
		connector.begin();
		final String query = "Some query";
		doThrow(new SesameDriverException()).when(centralMock).executeUpdate(query);
		try {
			connector.executeUpdate(query);
		} finally {
			verify(writeLock).lock();
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

	@Test
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
	public void testClose() throws Exception {
		assertTrue(connector.isOpen());
		connector.close();
		assertFalse(connector.isOpen());
	}

}
