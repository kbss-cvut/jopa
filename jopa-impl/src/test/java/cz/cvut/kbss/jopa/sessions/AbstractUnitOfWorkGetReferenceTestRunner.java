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
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.EntityNotFoundException;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.proxy.reference.EntityReferenceProxy;
import cz.cvut.kbss.jopa.proxy.reference.EntityReferenceProxyGenerator;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public abstract class AbstractUnitOfWorkGetReferenceTestRunner extends UnitOfWorkTestBase {

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
    void getReferenceThrowsEntityNotFoundExceptionWhenObjectIsScheduledForDeletion() {
        final OWLClassA existing = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(existing);
        assertThrows(EntityNotFoundException.class, () -> uow.getReference(OWLClassA.class, entityA.getUri(), descriptor));
    }

    @Test
    void getReferenceThrowsEntityExistsExceptionWhenIndividualIsAlreadyManagedAsDifferentIncompatibleType() {
        uow.registerExistingObject(entityA, descriptor);
        assertThrows(OWLEntityExistsException.class,
                () -> uow.getReference(OWLClassD.class, entityA.getUri(), descriptor));
    }

    @Test
    void getReferenceGetsReferenceFromStorageWhenItIsNotManaged() {
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(result);
        assertEquals(reference.getUri(), result.getUri());
        verify(storageMock).getReference(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor));
    }

    OWLClassA createReference(URI identifier) {
        final Class<? extends OWLClassA> proxyClass = new EntityReferenceProxyGenerator().generate(OWLClassA.class);
        try {
            final OWLClassA reference = proxyClass.getDeclaredConstructor().newInstance();
            final EntityReferenceProxy<OWLClassA> proxy = (EntityReferenceProxy<OWLClassA>) reference;
            proxy.setDescriptor(descriptor);
            proxy.setType(OWLClassA.class);
            proxy.setPersistenceContext(uow);
            proxy.setIdentifier(identifier);
            final Field idField = OWLClassA.class.getDeclaredField("uri");
            idField.setAccessible(true);
            idField.set(reference, identifier);
            return reference;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    void containsReturnsTrueForInstanceRetrievedUsingGetReference() {
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertTrue(uow.contains(result));
    }

    @Test
    void getStateReturnsManagedForInstanceRetrieveUsingGetReference() {
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertEquals(EntityState.MANAGED, uow.getState(result));
        assertEquals(EntityState.MANAGED, uow.getState(result, descriptor));
    }

    @Test
    void removeOfObjectRetrievedUsingGetReferenceEvictsCorrespondingInstanceFromCacheOnCommit() {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        uow.removeObject(result);
        uow.commit();
        verify(serverSessionStub.getLiveObjectCache()).evict(OWLClassA.class, reference.getUri(), descriptor.getSingleContext()
                                                                                                            .orElse(null));
    }

    @Test
    void changesToGetReferenceResultAreMergedIntoOriginalInCacheOnCommit() {
        when(transactionMock.isActive()).thenReturn(true);
        when(serverSessionStub.getLiveObjectCache()
                              .contains(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(true);
        when(serverSessionStub.getLiveObjectCache()
                              .get(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(entityA);
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(entityA);
        final OWLClassA a = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        final String strValue = "string value";
        a.setStringAttribute(strValue);
        uow.commit();
        assertEquals(strValue, entityA.getStringAttribute());
    }

    @Test
    void mergeDetachedMarksChangeRecordForAttributeWithGetReferenceResultAsPreventingCaching() {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA ref = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        final OWLClassD owner = new OWLClassD(Generators.createIndividualIdentifier());
        final OWLClassD toMerge = new OWLClassD(owner.getUri());
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(owner);
        when(storageMock.contains(owner.getUri(), OWLClassD.class, descriptor)).thenReturn(true);
        toMerge.setOwlClassA(ref);

        uow.mergeDetached(toMerge, descriptor);
        uow.commit();

        verify(serverSessionStub.getLiveObjectCache(), never()).add(eq(toMerge.getUri()), eq(owner), any());
    }
}
