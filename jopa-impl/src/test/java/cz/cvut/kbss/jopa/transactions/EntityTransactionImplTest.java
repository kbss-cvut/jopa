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
package cz.cvut.kbss.jopa.transactions;

import cz.cvut.kbss.jopa.exceptions.RollbackException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
public class EntityTransactionImplTest {

    @Mock
    private EntityTransactionWrapper wrapperMock;

    private EntityTransactionImpl transaction;

    @BeforeEach
    public void setUp() {
        transaction = new EntityTransactionImpl(wrapperMock);
    }

    @Test
    public void callingConstructorWithNullArgumentsThrowsNPX() {
        assertThrows(NullPointerException.class, () -> new EntityTransactionImpl(null));
    }

    @Test
    public void testBegin() {
        assertFalse(transaction.isActive());
        transaction.begin();
        assertTrue(transaction.isActive());
        verify(wrapperMock).begin();
    }

    @Test
    public void testBeginAlreadyActive() {
        assertFalse(transaction.isActive());
        transaction.begin();
        assertTrue(transaction.isActive());
        assertThrows(IllegalStateException.class, () -> transaction.begin());

    }

    @Test
    public void testCommit() {
        transaction.begin();
        transaction.commit();
        assertFalse(transaction.isActive());
        verify(wrapperMock).commit();
    }

    @Test
    public void testCommitNotActive() {
        assertThrows(IllegalStateException.class, () -> transaction.commit());
    }

    @Test
    public void testCommitRollbackOnly() {
        transaction.begin();
        transaction.setRollbackOnly();
        assertThrows(RollbackException.class, () -> transaction.commit());
    }

    @Test
    public void testRollback() {
        transaction.begin();
        transaction.rollback();
        assertFalse(transaction.isActive());
        verify(wrapperMock).rollback();
    }

    @Test
    public void testRollbackNotActive() {
        assertFalse(transaction.isActive());
        assertThrows(IllegalStateException.class, () -> transaction.rollback());
    }

    @Test
    public void testSetRollbackOnly() {
        transaction.begin();
        assertFalse(transaction.isRollbackOnly());
        transaction.setRollbackOnly();
        assertTrue(transaction.isRollbackOnly());
    }

    @Test
    public void testSetRollbackOnlyNotActive() {
        assertThrows(IllegalStateException.class, () -> transaction.setRollbackOnly());
    }

    @Test
    public void testGetRollbackOnlyNotActive() {
        assertThrows(IllegalStateException.class, () -> transaction.isRollbackOnly());
    }
}
