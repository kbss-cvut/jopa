package cz.cvut.kbss.jopa.transactions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.RollbackException;
import cz.cvut.kbss.jopa.model.AbstractEntityManager;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class EntityTransactionWrapperTest {

    @Mock
    private AbstractEntityManager em;

    @Mock
    private UnitOfWork uow;

    @InjectMocks
    private EntityTransactionWrapper sut;

    @Test
    void beginInitializesUoWAndNotifiesEntityManagerOfTransactionStart() {
        when(em.getCurrentPersistenceContext()).thenReturn(uow);
        final EntityTransaction transaction = sut.getTransaction();
        sut.begin();
        final InOrder inOrder = Mockito.inOrder(em);
        inOrder.verify(em).getCurrentPersistenceContext();
        inOrder.verify(em).transactionStarted(transaction);
        verify(uow).begin();
    }

    @Test
    void commitCommitsTransactionalUoW() {
        when(em.getCurrentPersistenceContext()).thenReturn(uow);
        sut.getTransaction();
        sut.begin();
        sut.commit();
        verify(uow).commit();
    }

     @Test
     void commitRemovesCurrentPersistenceContextFromEntityManagerWhenUoWCommitThrowsException() {
         when(em.getCurrentPersistenceContext()).thenReturn(uow);
         doThrow(new OWLPersistenceException("Commit exception.")).when(uow).commit();
         sut.getTransaction();
         sut.begin();
         assertThrows(RollbackException.class, () -> sut.commit());
         verify(em).removeCurrentPersistenceContext();
     }

    @Test
    void transactionFinishedNotifiesEntityManagerOfTransactionFinish() {
        when(em.getCurrentPersistenceContext()).thenReturn(uow);
        final EntityTransaction transaction = sut.getTransaction();
        sut.begin();
        sut.commit();
        sut.transactionFinished();
        verify(em).transactionFinished(transaction);
    }

    @Test
    void transactionFinishedReleasesCurrentUoWWhenConfiguredTo() {
        when(em.getCurrentPersistenceContext()).thenReturn(uow);
        when(uow.shouldReleaseAfterCommit()).thenReturn(true);
        final EntityTransaction transaction = sut.getTransaction();
        sut.begin();
        sut.commit();
        sut.transactionFinished();
        verify(em).removeCurrentPersistenceContext();
        verify(em).transactionFinished(transaction);
    }

    @Test
    void rollbackReleasesCurrentUoW() {
        when(em.getCurrentPersistenceContext()).thenReturn(uow);
        sut.getTransaction();
        sut.begin();
        sut.rollback();
        verify(em).removeCurrentPersistenceContext();
    }

    @Test
    void rollbackFollowedByTransactionFinishedDoesNotThrowException() {
        when(em.getCurrentPersistenceContext()).thenReturn(uow);
        final EntityTransaction transaction = sut.getTransaction();
        sut.begin();
        sut.rollback();
        sut.transactionFinished();
        verify(em).removeCurrentPersistenceContext();
        verify(em).transactionFinished(transaction);
    }
}
