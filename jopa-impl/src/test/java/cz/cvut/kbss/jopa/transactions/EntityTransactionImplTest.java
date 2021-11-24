/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.transactions;

import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.AbstractEntityManager;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class EntityTransactionImplTest {

    @Mock
    private EntityTransactionWrapper wrapperMock;
    @Mock
    private AbstractEntityManager emMock;
    @Mock
    private UnitOfWork uowMock;

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
        when(wrapperMock.getEntityManager()).thenReturn(emMock);
        assertFalse(transaction.isActive());
        transaction.begin();
        assertTrue(transaction.isActive());
        verify(emMock).transactionStarted(transaction);
    }

    @Test
    public void testBeginAlreadyActive() {
        when(wrapperMock.getEntityManager()).thenReturn(emMock);
        assertFalse(transaction.isActive());
        transaction.begin();
        assertTrue(transaction.isActive());
        assertThrows(IllegalStateException.class, () -> transaction.begin());

    }

    @Test
    public void testCommit() {
        when(wrapperMock.getEntityManager()).thenReturn(emMock);
        when(wrapperMock.getTransactionUOW()).thenReturn(uowMock);
        transaction.begin();
        transaction.commit();
        assertFalse(transaction.isActive());
        verify(emMock).transactionFinished(transaction);
    }

    @Test
    public void testCommitAndRelease() {
        when(wrapperMock.getEntityManager()).thenReturn(emMock);
        when(wrapperMock.getTransactionUOW()).thenReturn(uowMock);
        when(uowMock.shouldReleaseAfterCommit()).thenReturn(Boolean.TRUE);
        transaction.begin();
        transaction.commit();
        verify(emMock).removeCurrentPersistenceContext();
    }

    @Test
    public void testCommitNotActive() {
        assertThrows(IllegalStateException.class, () -> transaction.commit());
    }

    @Test
    public void testCommitWithException() {
        when(wrapperMock.getEntityManager()).thenReturn(emMock);
        when(wrapperMock.getTransactionUOW()).thenReturn(uowMock);
        doThrow(RuntimeException.class).when(uowMock).commit();
        transaction.begin();
        try {
            assertThrows(RuntimeException.class, () -> transaction.commit());
        } finally {
            verify(emMock).removeCurrentPersistenceContext();
        }
    }

    @Test
    public void testCommitRollbackOnly() {
        when(wrapperMock.getEntityManager()).thenReturn(emMock);
        when(wrapperMock.getTransactionUOW()).thenReturn(uowMock);
        transaction.begin();
        transaction.setRollbackOnly();
        assertThrows(RollbackException.class, () -> transaction.commit());
    }

    @Test
    public void testRollback() {
        when(wrapperMock.getEntityManager()).thenReturn(emMock);
        when(wrapperMock.getTransactionUOW()).thenReturn(uowMock);
        transaction.begin();
        transaction.rollback();
        assertFalse(transaction.isActive());
        verify(uowMock).rollback();
    }

    @Test
    public void testRollbackNotActive() {
        assertFalse(transaction.isActive());
        assertThrows(IllegalStateException.class, () -> transaction.rollback());
    }

    @Test
    public void testSetRollbackOnly() {
        when(wrapperMock.getEntityManager()).thenReturn(emMock);
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
