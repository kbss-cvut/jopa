/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class OnCommitChangePropagatingUnitOfWorkGetReferenceTest extends AbstractUnitOfWorkGetReferenceTestRunner {

    @BeforeEach
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected AbstractUnitOfWork initUnitOfWork() {
        return new OnCommitChangePropagatingUnitOfWork(serverSessionStub, new Configuration());
    }

    @Test
    void removeOfInstanceRetrievedUsingGetReferenceSchedulesItForDeletion() {
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertTrue(uow.contains(result));
        uow.removeObject(result);
        assertFalse(uow.contains(result));
        assertEquals(EntityState.REMOVED, uow.getState(result));
    }

    @Test
    void removeOfInstanceRetrievedUsingGetReferenceRemovesItFromStorageOnCommit() {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        uow.removeObject(result);
        verify(storageMock, never()).remove(entityA.getUri(), OWLClassA.class, descriptor);
        uow.commit();
        verify(storageMock).remove(entityA.getUri(), OWLClassA.class, descriptor);
    }

    @Test
    void changeOfAttributeValueOfInstanceRetrievedByGetReferenceIsPropagatedToStorageOnCommit() {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(entityA);
        final OWLClassA entity = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        final String newStringValue = "New string attribute value";
        entity.setStringAttribute(newStringValue);
        verify(storageMock, never()).merge(any(), eq(metamodelMocks.forOwlClassA().stringAttribute()), eq(descriptor));
        uow.commit();
        verify(storageMock).merge(any(), eq(metamodelMocks.forOwlClassA().stringAttribute()), eq(descriptor));
    }
}
