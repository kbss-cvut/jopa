/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.util;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.*;

public class TransactionTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private Transaction transaction;

    @Before
    public void setUp() throws Exception {
        this.transaction = new Transaction();
    }

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

    @Test
    public void beginAlreadyActiveIsIllegal() {
        thrown.expect(IllegalStateException.class);
        transaction.begin();
        assertTrue(transaction.isActive());
        transaction.begin();
    }

    @Test
    public void commitInactiveIsIllegal() {
        thrown.expect(IllegalStateException.class);
        transaction.begin();
        transaction.commit();
        transaction.afterCommit();
        // Now comes the second commit
        transaction.commit();
    }

    @Test
    public void rollbackCommittedIsIllegal() {
        thrown.expect(IllegalStateException.class);
        transaction.begin();
        transaction.commit();
        transaction.afterCommit();
        // Now comes the rollback
        transaction.rollback();
    }

    @Test
    public void afterRollbackWithoutProperRollbackIsIllegal() {
        thrown.expect(IllegalStateException.class);
        transaction.begin();
        transaction.afterRollback();
    }

    @Test
    public void afterCommitWithoutProperCommitIsIllegal() {
        thrown.expect(IllegalStateException.class);
        transaction.begin();
        transaction.afterCommit();
    }

    @Test
    public void commitUninitializedIsIllegal() {
        thrown.expect(IllegalStateException.class);
        transaction.commit();
    }

    @Test
    public void commitRolledBackIsIllegal() {
        thrown.expect(IllegalStateException.class);
        transaction.begin();
        transaction.rollback();
        transaction.commit();
    }

    @Test
    public void verifyActiveThrowsIllegalStateForInactiveTransaction() {
        thrown.expect(IllegalStateException.class);
        transaction.verifyActive();
    }
}
