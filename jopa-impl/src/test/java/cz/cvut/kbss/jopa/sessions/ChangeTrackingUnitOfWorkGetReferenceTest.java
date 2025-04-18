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
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ChangeTrackingUnitOfWorkGetReferenceTest extends AbstractUnitOfWorkGetReferenceTestRunner {

    @BeforeEach
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected AbstractUnitOfWork initUnitOfWork() {
        return new ChangeTrackingUnitOfWork(serverSessionStub, new Configuration());
    }

    @Test
    void attributeChangedPropagatesChangeOfInstanceRetrievedUsingGetReferenceIntoRepository() throws Exception {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        uow.attributeChanged(result, OWLClassA.getStrAttField());
        verify(storageMock).merge(result, metamodelMocks.forOwlClassA().stringAttribute(), descriptor);
    }

    @Test
    void attributeChangedDoesNotRegisterChangeForMergingIntoCachedOriginalForInstanceRetrievedUsingGetReference() throws Exception {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        uow.attributeChanged(result, OWLClassA.getStrAttField());
        assertFalse(uow.uowChangeSet.hasChanges());
    }

    @Test
    void removeRemovesInstanceRetrievedUsingGetReferenceFromRepository() {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA reference = createReference(entityA.getUri());
        when(storageMock.getReference(any(LoadingParameters.class))).thenReturn(reference);
        final OWLClassA result = uow.getReference(OWLClassA.class, entityA.getUri(), descriptor);
        assertTrue(uow.contains(result));
        uow.removeObject(result);
        assertFalse(uow.contains(result));
        assertEquals(EntityState.REMOVED, uow.getState(result));
        verify(storageMock).remove(reference.getUri(), OWLClassA.class, descriptor);
    }
}
