package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.EntityManagerImpl;
import cz.cvut.kbss.jopa.model.LoadState;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class UnitOfWorkGetReferenceTest extends UnitOfWorkTestBase {

    @BeforeEach
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Test
    void getReferenceReturnsExistingCloneWhenItIsAlreadyManaged() {
        final OWLClassA existing = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(result);
        assertSame(existing, result);
    }

    @Test
    void getReferenceReturnsExistingNewlyRegisteredObject() {
        uow.registerNewObject(entityA, descriptor);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(result);
        assertSame(entityA, result);
    }

    @Test
    void getReferenceReturnsNullWhenObjectIsScheduledForDeletion() {
        final OWLClassA existing = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(existing);
        assertNull(uow.getReference(OWLClassA.class, entityA.getUri(), descriptor));
    }

    @Test
    void getReferenceThrowsEntityExistsExceptionWhenIndividualIsAlreadyManagedAsDifferentIncompatibleType() {
        uow.registerExistingObject(entityA, descriptor);
        assertThrows(OWLEntityExistsException.class,
                () -> uow.getReference(OWLClassD.class, entityA.getUri(), descriptor));
    }

    @Test
    void getReferenceLoadsReferenceFromStorageWhenItIsNotManaged() {
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertEquals(reference, result);
        verify(storageMock).getReference(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor));
    }

    @Test
    void containsReturnsTrueForInstanceRetrievedUsingGetReference() {
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertTrue(uow.contains(result));
    }

    @Test
    void getStateReturnsManagedForInstanceRetrieveUsingGetReference() {
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertEquals(EntityManagerImpl.State.MANAGED, uow.getState(result));
        assertEquals(EntityManagerImpl.State.MANAGED, uow.getState(result, descriptor));
    }

    @Test
    void removeOfInstanceRetrievedUsingGetReferenceSchedulesItForDeletion() {
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertTrue(uow.contains(result));
        uow.removeObject(result);
        assertFalse(uow.contains(result));
        assertEquals(EntityManagerImpl.State.REMOVED, uow.getState(result));
        verify(storageMock).remove(entityA.getUri(), OWLClassA.class, descriptor);
    }

    @Test
    void loadEntityFieldLoadsValueOfAttributeOfInstanceRetrievedUsingGetReference() throws Exception {
        final OWLClassL reference = new OWLClassL(entityL.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassL result = uow.getReference(OWLClassL.class, entityL.getUri(), descriptor);
        uow.loadEntityField(result, OWLClassL.getSetField());
        verify(storageMock).loadFieldValue(result, OWLClassL.getSetField(), descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(result, OWLClassL.getSetField().getName()));
    }

    @Test
    void loadEntityFieldDoesNothingWhenLazilyLoadedAttributeOfInstanceRetrievedUsingGetReferenceIsAlreadyLoaded()
            throws Exception {
        final OWLClassL reference = new OWLClassL(entityL.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassL result = uow.getReference(OWLClassL.class, entityL.getUri(), descriptor);
        uow.loadEntityField(result, OWLClassL.getSetField());
        // Call it twice. Storage should be called only once
        uow.loadEntityField(result, OWLClassL.getSetField());
        verify(storageMock).loadFieldValue(result, OWLClassL.getSetField(), descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(result, OWLClassL.getSetField().getName()));
    }
}
