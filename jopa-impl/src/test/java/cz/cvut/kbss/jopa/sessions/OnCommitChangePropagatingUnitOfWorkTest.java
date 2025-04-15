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

import cz.cvut.kbss.jopa.environment.NoopInstantiableTypeGenerator;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelFactory;
import cz.cvut.kbss.jopa.exceptions.AttributeModificationForbiddenException;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class OnCommitChangePropagatingUnitOfWorkTest extends ReadWriteUnitOfWorkTest {

    @BeforeAll
    static void setUpBeforeAll() {
        MetamodelFactory.setInstantiableTypeGenerator(NoopInstantiableTypeGenerator.INSTANCE);
    }

    @BeforeEach
    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected AbstractUnitOfWork initUnitOfWork() {
        return new OnCommitChangePropagatingUnitOfWork(serverSessionStub, new Configuration());
    }

    @AfterAll
    static void tearDownAfterAll() {
        MetamodelFactory.reset();
    }

    @Test
    void commitToStorageCalculatesChangesToExistingObjectsAndPropagatesThemToStorage() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        clone.setStringAttribute("new string value");
        assertTrue(uow.uowChangeSet.getExistingObjectsChanges().isEmpty());
        uow.commit();
        verify(storageMock).merge(clone, metamodelMocks.forOwlClassA().stringAttribute(), descriptor);
    }

    @Test
    void commitToStorageDeletesRemovedObjectsFromStorage() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(clone);
        verify(storageMock, never()).remove(clone.getUri(), OWLClassA.class, descriptor);
        uow.commit();
        verify(storageMock).remove(clone.getUri(), OWLClassA.class, descriptor);
    }

    @Test
    void removeObjectRegistersObjectForDeletion() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(clone);
        verify(storageMock, never()).remove(clone.getUri(), OWLClassA.class, descriptor);
        assertTrue(uow.deletedObjects.containsKey(clone));
    }

    @Test
    void mergeDetachedRegistersChangesToSpecifiedObjectAndReturnsManagedClone() {
        when(storageMock.contains(entityA.getUri(), OWLClassA.class, descriptor)).thenReturn(true);
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(entityA);
        defaultLoadStateDescriptor(entityA);
        final OWLClassA toMerge = new OWLClassA(entityA.getUri());
        toMerge.setStringAttribute("Different string");
        toMerge.setTypes(Generators.generateTypes(2));

        final OWLClassA result = uow.mergeDetached(toMerge, descriptor);
        assertNotNull(result);
        assertEquals(entityA.getUri(), result.getUri());
        assertEquals(toMerge.getStringAttribute(), result.getStringAttribute());
        assertEquals(toMerge.getTypes(), result.getTypes());
        assertTrue(uow.contains(result));
        assertTrue(uow.uowChangeSet.hasChanges());
        final ObjectChangeSet changeSet = uow.uowChangeSet.getExistingObjectChanges(uow.getOriginal(result));
        assertTrue(changeSet.getChanges().stream()
                            .anyMatch(r -> r.getAttribute().equals(metamodelMocks.forOwlClassA().stringAttribute())));
        assertTrue(changeSet.getChanges().stream()
                            .anyMatch(r -> r.getAttribute().equals(metamodelMocks.forOwlClassA().typesSpec())));
        verify(storageMock, never()).merge(any(), any(), any());
    }

    @Test
    void commitThrowsAttributeModificationForbiddenExceptionWhenChangeConcernsLexicalValueAttribute() {
        final OWLClassM original = new OWLClassM();
        original.initializeTestValues(true);
        defaultLoadStateDescriptor(original);
        final OWLClassM clone = (OWLClassM) uow.registerExistingObject(original, descriptor);
        clone.setLexicalForm("Cannot change");
        assertThrows(AttributeModificationForbiddenException.class, () -> uow.commit());
    }

    @Test
    void commitRegistersChangesDoneByPreUpdateCallback() {
        final OWLClassU original = new OWLClassU(Generators.createIndividualIdentifier());
        when(storageMock.contains(original.getId(), OWLClassU.class, descriptor)).thenReturn(true);
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(original);
        defaultLoadStateDescriptor(original);
        final OWLClassU toMerge = new OWLClassU(original.getId());
        toMerge.setSingularStringAtt(MultilingualString.create("Test", "en"));
        final OWLClassU merged = uow.mergeDetached(toMerge, descriptor);
        uow.commit();
        verify(storageMock).merge(merged, metamodelMocks.forOwlClassU().uModified(), descriptor);
    }
}
