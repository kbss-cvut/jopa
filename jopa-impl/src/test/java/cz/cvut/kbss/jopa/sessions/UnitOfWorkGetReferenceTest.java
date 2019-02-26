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
import static org.mockito.Mockito.*;

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

    @Test
    void attributeChangedPropagatesChangeOfInstanceRetrievedUsingGetReferenceIntoRepository() throws Exception {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        uow.attributeChanged(result, OWLClassA.getStrAttField());
        verify(storageMock).merge(result, OWLClassA.getStrAttField(), descriptor);
    }

    @Test
    void attributeChangedDoesNotRegisterChangeForInstanceRetrievedUsingGetReference() throws Exception {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        uow.attributeChanged(result, OWLClassA.getStrAttField());
        assertFalse(uow.getUowChangeSet().hasChanges());
    }

    @Test
    void removeRemovesInstanceRetrievedUsingGetReferenceFromRepository() {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertTrue(uow.contains(result));
        uow.removeObject(result);
        assertFalse(uow.contains(result));
        verify(storageMock).remove(reference.getUri(), OWLClassA.class, descriptor);
    }

    @Test
    void removeCreatesRemoveChangeOnCommitForInstanceRetrievedUsingGetReference() {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        uow.removeObject(result);
        uow.commit();
        verify(cacheManagerMock).evict(OWLClassA.class, reference.getUri(), descriptor.getContext());
    }

    @Test
    void getReferenceLoadsOriginalFromSecondLevelCacheWhenPresent() {
        when(cacheManagerMock.contains(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(true);
        when(cacheManagerMock.get(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(entityA);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertEquals(entityA, uow.getOriginal(result));
    }

    @Test
    void changesToGetReferenceResultAreMergedIntoOriginalInCache() {
        when(transactionMock.isActive()).thenReturn(true);
        when(cacheManagerMock.contains(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(true);
        when(cacheManagerMock.get(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(entityA);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA a = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        final String strValue = "string value";
        a.setStringAttribute(strValue);
        uow.commit();
        assertEquals(strValue, entityA.getStringAttribute());
    }

    @Test
    void uowCommitEvictsInstanceRetrievedUsingGetReferenceFromCacheWhenItWasNotPresentThereOnRetrieval() {
        when(transactionMock.isActive()).thenReturn(true);
        when(cacheManagerMock.contains(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(false);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA a = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        final String strValue = "string value";
        a.setStringAttribute(strValue);
        uow.commit();
        verify(cacheManagerMock).evict(OWLClassA.class, entityA.getUri(), descriptor.getContext());
    }
}
