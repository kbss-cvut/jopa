package cz.cvut.kbss.ontodriver.sesame;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

public class TransactionTest {

	private Transaction transaction;

	@Before
	public void setUp() throws Exception {
		this.transaction = new Transaction();
	}

	// The following tests cover the basic transaction state transitions

	@Test
	public void testBegin() {
		transaction.begin();
		assertTrue(transaction.isActive());
		assertEquals(TransactionState.ACTIVE, transaction.getState());
	}

	@Test
	public void testCommit() {
		transaction.begin();
		transaction.commit();
		assertEquals(TransactionState.PARTIALLY_COMMITTED, transaction.getState());
	}

	@Test
	public void testAfterCommit() {
		transaction.begin();
		transaction.commit();
		assertEquals(TransactionState.PARTIALLY_COMMITTED, transaction.getState());
		transaction.afterCommit();
		assertEquals(TransactionState.COMMITTED, transaction.getState());
		assertFalse(transaction.isActive());
	}

	@Test
	public void testFailDuringCommit() {
		transaction.begin();
		transaction.commit();
		assertEquals(TransactionState.PARTIALLY_COMMITTED, transaction.getState());
		transaction.rollback();
		assertEquals(TransactionState.FAILED, transaction.getState());
	}

	@Test
	public void testRollback() {
		transaction.begin();
		transaction.rollback();
		assertEquals(TransactionState.FAILED, transaction.getState());
		assertFalse(transaction.isActive());
	}

	@Test
	public void testAfterRollback() {
		transaction.begin();
		transaction.rollback();
		assertEquals(TransactionState.FAILED, transaction.getState());
		transaction.afterRollback();
		assertEquals(TransactionState.ABORTED, transaction.getState());
		assertFalse(transaction.isActive());
	}

	// Several invalid transition tests follow

	@Test(expected = IllegalStateException.class)
	public void testBeginTwice() {
		transaction.begin();
		assertTrue(transaction.isActive());
		transaction.begin();
	}

	@Test(expected = IllegalStateException.class)
	public void testCommitInactive() {
		transaction.begin();
		transaction.commit();
		transaction.afterCommit();
		// Now comes the second commit
		transaction.commit();
	}

	@Test(expected = IllegalStateException.class)
	public void rollbackCommitted() {
		transaction.begin();
		transaction.commit();
		transaction.afterCommit();
		// Now comes the rollback
		transaction.rollback();
	}

	@Test(expected = IllegalStateException.class)
	public void testAfterRollbackWithoutProperRollback() {
		transaction.begin();
		transaction.afterRollback();
	}

	@Test(expected = IllegalStateException.class)
	public void testAfterCommitWithoutProperCommit() {
		transaction.begin();
		transaction.afterCommit();
	}

	@Test(expected = IllegalStateException.class)
	public void testCommitUninitialized() {
		transaction.commit();
	}

	@Test(expected = IllegalStateException.class)
	public void testCommitedRolledBack() {
		transaction.begin();
		transaction.rollback();
		transaction.commit();
	}
}
