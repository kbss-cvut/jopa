/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
@Disabled
public class UnitOfWorkGetReferenceTest extends UnitOfWorkTestBase {

    // TODO Support ChangeTrackingUoW and OnCommitChangePropagatingUoW

    @BeforeEach
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected AbstractUnitOfWork initUnitOfWork() {
        return new ChangeTrackingUnitOfWork(serverSessionStub, new Configuration());
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
        assertNotNull(result);
        assertEquals(reference.getUri(), result.getUri());
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
        assertEquals(EntityState.MANAGED, uow.getState(result));
        assertEquals(EntityState.MANAGED, uow.getState(result, descriptor));
    }

    @Test
    void removeOfInstanceRetrievedUsingGetReferenceSchedulesItForDeletion() {
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertTrue(uow.contains(result));
        uow.removeObject(result);
        assertFalse(uow.contains(result));
        assertEquals(EntityState.REMOVED, uow.getState(result));
        verify(storageMock).remove(entityA.getUri(), OWLClassA.class, descriptor);
    }

    @Test
    void loadEntityFieldLoadsValueOfAttributeOfInstanceRetrievedUsingGetReference() throws Exception {
        final OWLClassL reference = new OWLClassL(entityL.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassL result = uow.getReference(OWLClassL.class, entityL.getUri(), descriptor);
        uow.loadEntityField(result, metamodelMocks.forOwlClassL().setAttribute());
        verify(storageMock).loadFieldValue(result, metamodelMocks.forOwlClassL().setAttribute(), descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(result, OWLClassL.getSetField().getName()));
    }

    @Test
    void loadEntityFieldDoesNothingWhenLazilyLoadedAttributeOfInstanceRetrievedUsingGetReferenceIsAlreadyLoaded()
            throws Exception {
        final OWLClassL reference = new OWLClassL(entityL.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassL result = uow.getReference(OWLClassL.class, entityL.getUri(), descriptor);
        uow.loadEntityField(result, metamodelMocks.forOwlClassL().setAttribute());
        // Call it twice. Storage should be called only once
        uow.loadEntityField(result, metamodelMocks.forOwlClassL().setAttribute());
        verify(storageMock).loadFieldValue(result, metamodelMocks.forOwlClassL().setAttribute(), descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(result, OWLClassL.getSetField().getName()));
    }

    @Test
    void attributeChangedPropagatesChangeOfInstanceRetrievedUsingGetReferenceIntoRepository() throws Exception {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        uow.attributeChanged(result, OWLClassA.getStrAttField());
        verify(storageMock).merge(result, metamodelMocks.forOwlClassA().stringAttribute(), descriptor);
    }

    @Test
    void attributeChangedDoesNotRegisterChangeForInstanceRetrievedUsingGetReference() throws Exception {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        uow.attributeChanged(result, OWLClassA.getStrAttField());
        assertFalse(uow.uowChangeSet.hasChanges());
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
        verify(serverSessionStub.getLiveObjectCache()).evict(OWLClassA.class, reference.getUri(), descriptor.getSingleContext().orElse(null));
    }

    @Test
    void getReferenceLoadsOriginalFromSecondLevelCacheWhenPresent() {
        when(serverSessionStub.getLiveObjectCache().contains(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(true);
        when(serverSessionStub.getLiveObjectCache().get(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(entityA);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertEquals(entityA, uow.getOriginal(result));
    }

    @Test
    void changesToGetReferenceResultAreMergedIntoOriginalInCache() {
        when(transactionMock.isActive()).thenReturn(true);
        when(serverSessionStub.getLiveObjectCache().contains(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(true);
        when(serverSessionStub.getLiveObjectCache().get(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(entityA);
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
        when(serverSessionStub.getLiveObjectCache().contains(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(false);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA a = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        final String strValue = "string value";
        a.setStringAttribute(strValue);
        uow.commit();
        verify(serverSessionStub.getLiveObjectCache()).evict(OWLClassA.class, entityA.getUri(), descriptor.getSingleContext().orElse(null));
    }

    @Test
    void attributeChangeSetsChangeRecordToPreventCachingWhenNewValueWasRetrievedUsingGetReference() throws Exception {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        final OWLClassD owner = new OWLClassD(Generators.createIndividualIdentifier());
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(owner);
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassD changed = uow.readObject(OWLClassD.class, owner.getUri(), descriptor);
        final OWLClassA ref = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        changed.setOwlClassA(ref);

        uow.attributeChanged(changed, OWLClassD.getOwlClassAField());

        final ObjectChangeSet changeSet = uow.uowChangeSet.getExistingObjectChanges(owner);
        assertFalse(changeSet.getChanges().isEmpty());
        final Optional<ChangeRecord> changeRecord =
                changeSet.getChanges().stream().filter(chr -> chr.getNewValue().equals(ref)).findFirst();
        assertTrue(changeRecord.isPresent());
        assertTrue(changeRecord.get().doesPreventCaching());
    }

    @Test
    void mergeDetachedMarksChangeRecordForAttributeWithGetReferenceResultAsPreventingCaching() {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = new OWLClassA(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA ref = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        final OWLClassD owner = new OWLClassD(Generators.createIndividualIdentifier());
        final OWLClassD toMerge = new OWLClassD(owner.getUri());
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(owner);
        when(storageMock.contains(owner.getUri(), OWLClassD.class, descriptor)).thenReturn(true);
        toMerge.setOwlClassA(ref);

        uow.mergeDetached(toMerge, descriptor);

        final ObjectChangeSet changeSet = uow.uowChangeSet.getExistingObjectChanges(owner);
        assertFalse(changeSet.getChanges().isEmpty());
        final Optional<ChangeRecord> changeRecord =
                changeSet.getChanges().stream().filter(chr -> chr.getNewValue().equals(ref)).findFirst();
        assertTrue(changeRecord.isPresent());
        assertTrue(changeRecord.get().doesPreventCaching());
    }
}
