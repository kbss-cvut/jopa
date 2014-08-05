package cz.cvut.kbss.jopa.transactions;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.owlapi.AbstractEntityManager;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

public class EntityTransactionImplTest {

	@Mock
	private EntityTransactionWrapper wrapperMock;
	@Mock
	private AbstractEntityManager emMock;
	@Mock
	private UnitOfWork uowMock;

	private EntityTransactionImpl transaction;

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(wrapperMock.getEntityManager()).thenReturn(emMock);
		when(wrapperMock.getTransactionUOW()).thenReturn(uowMock);
		this.transaction = new EntityTransactionImpl(wrapperMock);
	}

	@Test(expected = NullPointerException.class)
	public void testEntityTransactionImpl() {
		final EntityTransactionImpl t = new EntityTransactionImpl(null);
		fail("This line should not have been reached.");
		assert t == null;
	}

	@Test
	public void testBegin() {
		assertFalse(transaction.isActive());
		transaction.begin();
		assertTrue(transaction.isActive());
		verify(emMock).transactionStarted(transaction);
	}

	@Test(expected = IllegalStateException.class)
	public void testBeginAlreadyActive() {
		assertFalse(transaction.isActive());
		transaction.begin();
		assertTrue(transaction.isActive());
		transaction.begin();
		fail("This line should not have been reached.");

	}

	@Test
	public void testCommit() {
		transaction.begin();
		transaction.commit();
		assertFalse(transaction.isActive());
		verify(emMock).transactionFinished(transaction);
	}

	@Test
	public void testCommitAndRelease() {
		when(uowMock.shouldReleaseAfterCommit()).thenReturn(Boolean.TRUE);
		transaction.begin();
		transaction.commit();
		verify(emMock).removeCurrentPersistenceContext();
	}

	@Test(expected = IllegalStateException.class)
	public void testCommitNotActive() {
		transaction.commit();
		fail("This line should not have been reached.");
	}

	@Test(expected = RuntimeException.class)
	public void testCommitWithException() {
		doThrow(RuntimeException.class).when(uowMock).commit();
		transaction.begin();
		try {
			transaction.commit();
		} finally {
			verify(emMock).removeCurrentPersistenceContext();
		}
	}

	@Test(expected = RollbackException.class)
	public void testCommitRollbackOnly() {
		transaction.begin();
		transaction.setRollbackOnly();
		transaction.commit();
		fail("This line should not have been reached.");
	}

	@Test
	public void testRollback() {
		transaction.begin();
		transaction.rollback();
		assertFalse(transaction.isActive());
		verify(uowMock).rollback();
	}

	@Test(expected = IllegalStateException.class)
	public void testRollbackNotActive() {
		assertFalse(transaction.isActive());
		transaction.rollback();
		fail("This line should not have been reached.");
	}

	@Test
	public void testSetRollbackOnly() {
		transaction.begin();
		assertFalse(transaction.isRollbackOnly());
		transaction.setRollbackOnly();
		assertTrue(transaction.isRollbackOnly());
	}

	@Test(expected = IllegalStateException.class)
	public void testSetRollbackOnlyNotActive() {
		transaction.setRollbackOnly();
		fail("This line should not have been reached.");
	}

	@Test(expected = IllegalStateException.class)
	public void testGetRollbackOnlyNotActive() {
		transaction.isRollbackOnly();
		fail("This line should not have been reached.");
	}

	@Test
	public void testFinalize() throws Throwable {
		transaction.begin();
		transaction.finalize();
		verify(uowMock).rollback();
	}
}
