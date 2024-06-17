/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.util;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class TransactionTest {

    private Transaction transaction;

    @BeforeEach
    void setUp() {
        this.transaction = new Transaction();
    }

    @Test
    void testBegin() {
        transaction.begin();
        assertTrue(transaction.isActive());
        assertEquals(TransactionState.ACTIVE, transaction.getState());
    }

    @Test
    void testCommit() {
        transaction.begin();
        transaction.commit();
        assertEquals(TransactionState.PARTIALLY_COMMITTED, transaction.getState());
    }

    @Test
    void testAfterCommit() {
        transaction.begin();
        transaction.commit();
        assertEquals(TransactionState.PARTIALLY_COMMITTED, transaction.getState());
        transaction.afterCommit();
        assertEquals(TransactionState.COMMITTED, transaction.getState());
        assertFalse(transaction.isActive());
    }

    @Test
    void testFailDuringCommit() {
        transaction.begin();
        transaction.commit();
        assertEquals(TransactionState.PARTIALLY_COMMITTED, transaction.getState());
        transaction.rollback();
        assertEquals(TransactionState.FAILED, transaction.getState());
    }

    @Test
    void testRollback() {
        transaction.begin();
        transaction.rollback();
        assertEquals(TransactionState.FAILED, transaction.getState());
        assertFalse(transaction.isActive());
    }

    @Test
    void testAfterRollback() {
        transaction.begin();
        transaction.rollback();
        assertEquals(TransactionState.FAILED, transaction.getState());
        transaction.afterRollback();
        assertEquals(TransactionState.ABORTED, transaction.getState());
        assertFalse(transaction.isActive());
    }

    @Test
    void beginAlreadyActiveIsIllegal() {
        transaction.begin();
        assertTrue(transaction.isActive());
        assertThrows(IllegalStateException.class, () -> transaction.begin());
    }

    @Test
    void beginPartiallyCommittedIsIllegal() {
        transaction.begin();
        transaction.commit();
        assertThrows(IllegalStateException.class, () -> transaction.begin());
    }

    @Test
    void commitInactiveIsIllegal() {
        transaction.begin();
        transaction.commit();
        transaction.afterCommit();
        // Now comes the second commit
        assertThrows(IllegalStateException.class, () -> transaction.commit());
    }

    @Test
    void rollbackInactiveIsIllegal() {
        assertFalse(transaction.isActive());
        assertThrows(IllegalStateException.class, () -> transaction.rollback());
    }

    @Test
    void rollbackCommittedIsIllegal() {
        transaction.begin();
        transaction.commit();
        transaction.afterCommit();
        // Now comes the rollback
        assertThrows(IllegalStateException.class, () -> transaction.rollback());
    }

    @Test
    void afterRollbackWithoutProperRollbackIsIllegal() {
        transaction.begin();
        assertThrows(IllegalStateException.class, () -> transaction.afterRollback());
    }

    @Test
    void afterCommitWithoutProperCommitIsIllegal() {
        transaction.begin();
        assertThrows(IllegalStateException.class, () -> transaction.afterCommit());
    }

    @Test
    void commitUninitializedIsIllegal() {
        assertThrows(IllegalStateException.class, () -> transaction.commit());
    }

    @Test
    void commitRolledBackIsIllegal() {
        transaction.begin();
        transaction.rollback();
        assertThrows(IllegalStateException.class, () -> transaction.commit());
    }

    @Test
    void verifyActiveThrowsIllegalStateForInactiveTransaction() {
        assertThrows(IllegalStateException.class, () -> transaction.verifyActive());
    }

    @Test
    void instancesAreEqualWhenTheyHaveSameState() {
        transaction.begin();
        final Transaction other = new Transaction();
        other.begin();
        assertEquals(transaction, other);
        assertEquals(transaction.hashCode(), other.hashCode());
        transaction.commit();
        assertNotEquals(transaction, other);
        assertNotEquals(transaction.hashCode(), other.hashCode());
    }
}
