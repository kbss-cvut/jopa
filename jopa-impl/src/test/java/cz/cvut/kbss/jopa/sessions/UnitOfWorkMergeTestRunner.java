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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassF;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.InferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.change.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

abstract class UnitOfWorkMergeTestRunner extends UnitOfWorkTestBase {

    OWLClassA mergeDetached() {
        when(storageMock.contains(entityA.getUri(), entityA.getClass(), descriptor)).thenReturn(Boolean.TRUE);
        final OWLClassA orig = new OWLClassA();
        orig.setUri(entityA.getUri());
        orig.setStringAttribute("oldStringAttribute");
        orig.setTypes(new HashSet<>(entityA.getTypes()));
        final Iterator<String> it = orig.getTypes().iterator();
        it.next();
        it.remove();
        when(storageMock.find(any())).thenReturn(orig);
        defaultLoadStateDescriptor(orig);

        return uow.mergeDetached(entityA, descriptor);
    }

    @Test
    void mergeDetachedEvictsInstanceFromCache() {
        when(serverSessionStub.getLiveObjectCache().contains(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(true);
        mergeDetached();
        verify(serverSessionStub.getLiveObjectCache()).evict(OWLClassA.class, entityA.getUri(), CONTEXT_URI);
    }

    @Test
    void mergeDetachedRegistersNewObjectWhenItDoesNotExist() {
        when(storageMock.contains(entityA.getUri(), entityA.getClass(), descriptor)).thenReturn(false);
        assertFalse(uow.contains(entityA));
        defaultLoadStateDescriptor(entityA);
        final OWLClassA res = uow.mergeDetached(entityA, descriptor);
        assertNotNull(res);
        assertSame(entityA, res);
        assertTrue(uow.isObjectNew(res));
    }

    @Test
    void mergeRegistersChangesInUoWChangeSet() throws Exception {
        final OWLClassA clone = new OWLClassA();
        clone.setUri(entityA.getUri());
        // These two attributes will be changed
        clone.setStringAttribute("changedStringAttribute");
        clone.setTypes(Collections.emptySet());
        when(storageMock.contains(entityA.getUri(), OWLClassA.class, descriptor)).thenReturn(true);
        when(storageMock.find(any())).thenReturn(entityA);
        defaultLoadStateDescriptor(entityA);
        uow.mergeDetached(clone, descriptor);

        assertTrue(uow.hasChanges());
        final UnitOfWorkChangeSet changeSet = uow.uowChangeSet;
        final ObjectChangeSet objectChanges = changeSet.getExistingObjectChanges(entityA);
        assertNotNull(objectChanges);
        assertEquals(2, objectChanges.getChanges().size());
        final String strAttName = OWLClassA.getStrAttField().getName();
        final Optional<ChangeRecord> rOne = objectChanges.getChanges().stream()
                                                         .filter(ch -> ch.getAttribute().getName().equals(strAttName))
                                                         .findAny();
        assertTrue(rOne.isPresent());
        assertEquals(clone.getStringAttribute(), rOne.get().getNewValue());
        final String typesAttName = OWLClassA.getTypesField().getName();
        final Optional<ChangeRecord> rTwo = objectChanges.getChanges().stream()
                                                         .filter(ch -> ch.getAttribute().getName().equals(typesAttName))
                                                         .findAny();
        assertTrue(rTwo.isPresent());
        assertEquals(clone.getTypes(), rTwo.get().getNewValue());
    }

    @Test
    void mergeReturnsInstanceWithReferencesWithOriginalValues() {
        final OWLClassA aOriginal = new OWLClassA(entityA.getUri());
        aOriginal.setStringAttribute(entityA.getStringAttribute());
        aOriginal.setTypes(new HashSet<>(entityA.getTypes()));
        final OWLClassD dOriginal = new OWLClassD(entityD.getUri());
        dOriginal.setOwlClassA(aOriginal);
        entityA.setStringAttribute("differentString");
        entityA.getTypes().add(Vocabulary.CLASS_BASE + "addedType");
        when(storageMock.contains(entityD.getUri(), OWLClassD.class, descriptor)).thenReturn(true);
        final LoadingParameters<OWLClassD> dParams = new LoadingParameters<>(OWLClassD.class, dOriginal.getUri(),
                                                                             descriptor, true);
        when(storageMock.find(dParams)).thenReturn(dOriginal);
        defaultLoadStateDescriptor(dOriginal, aOriginal);

        final OWLClassD result = uow.mergeDetached(entityD, descriptor);
        assertEquals(aOriginal.getStringAttribute(), result.getOwlClassA().getStringAttribute());
        assertEquals(aOriginal.getTypes(), result.getOwlClassA().getTypes());
    }

    @Test
    void mergeReturnsInstanceWithUpdatedReferenceWhenItWasChangedInTheDetachedObject() {
        final OWLClassA aOriginal = Generators.generateOwlClassAInstance();
        final OWLClassD dOriginal = new OWLClassD(entityD.getUri());
        dOriginal.setOwlClassA(aOriginal);
        when(storageMock.contains(entityD.getUri(), OWLClassD.class, descriptor)).thenReturn(true);
        final LoadingParameters<OWLClassD> dParams = new LoadingParameters<>(OWLClassD.class, dOriginal.getUri(),
                                                                             descriptor, true);
        when(storageMock.find(dParams)).thenReturn(dOriginal);
        defaultLoadStateDescriptor(dOriginal, aOriginal);

        final OWLClassD result = uow.mergeDetached(entityD, descriptor);
        assertEquals(entityA.getUri(), result.getOwlClassA().getUri());
        assertEquals(entityA.getStringAttribute(), result.getOwlClassA().getStringAttribute());
        assertEquals(entityA.getTypes(), result.getOwlClassA().getTypes());
    }

    @Test
    void mergeMergesChangesIntoExistingManagedInstanceAndReturnsIt() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        final OWLClassA detached = new OWLClassA(managed.getUri());
        detached.setTypes(new HashSet<>(managed.getTypes()));
        final String detachedString = "detachedStringAttribute";
        detached.setStringAttribute(detachedString);
        when(storageMock.contains(managed.getUri(), OWLClassA.class, descriptor)).thenReturn(true);

        final OWLClassA result = uow.mergeDetached(detached, descriptor);
        assertSame(managed, result);
        assertEquals(detachedString, result.getStringAttribute());
    }

    @Test
    void mergeDoesNotAddChangeSetToUoWChangeSetWhenItContainsNoChanges() {
        defaultLoadStateDescriptor(entityA, entityD);
        final OWLClassD managed = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        when(transactionMock.isActive()).thenReturn(true);
        when(storageMock.contains(entityD.getUri(), OWLClassD.class, descriptor)).thenReturn(true);
        final OWLClassD detached = new OWLClassD(managed.getUri());
        detached.setOwlClassA(entityA);

        uow.mergeDetached(detached, descriptor);
        assertNull(uow.uowChangeSet.getExistingObjectChanges(entityD));
    }

    @Test
    void mergeDetachedEvictsClassesPossiblyReferencingMergedTypeFromCache() {
        when(metamodelMock.getReferringTypes(OWLClassA.class)).thenReturn(
                new HashSet<>(Arrays.asList(OWLClassD.class, OWLClassC.class)));
        defaultLoadStateDescriptor(entityA);
        final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        final OWLClassA detached = new OWLClassA(managed.getUri());
        detached.setTypes(new HashSet<>(managed.getTypes()));
        final String detachedString = "detachedStringAttribute";
        detached.setStringAttribute(detachedString);
        when(storageMock.contains(managed.getUri(), OWLClassA.class, descriptor)).thenReturn(true);
        when(serverSessionStub.getLiveObjectCache().contains(OWLClassA.class, entityA.getUri(), descriptor)).thenReturn(true);

        uow.mergeDetached(detached, descriptor);
        verify(serverSessionStub.getLiveObjectCache()).evict(OWLClassA.class, entityA.getUri(), descriptor.getSingleContext().orElse(null));
        verify(serverSessionStub.getLiveObjectCache()).evict(OWLClassD.class);
        verify(serverSessionStub.getLiveObjectCache()).evict(OWLClassC.class);
    }

    @Test
    void mergeDetachedThrowsInferredAttributeModifiedExceptionOnChangeToInferredAttributeValue() {
        final OWLClassF original = new OWLClassF(Generators.createIndividualIdentifier());
        original.setSecondStringAttribute("Original value");
        final OWLClassF detached = new OWLClassF(original.getUri());
        detached.setSecondStringAttribute("Changed value");
        when(transactionMock.isActive()).thenReturn(true);
        when(storageMock.contains(detached.getUri(), OWLClassF.class, descriptor)).thenReturn(true);
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(original);
        defaultLoadStateDescriptor(original);
        final Assertion assertion =
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_f_stringAttribute), true);
        doAnswer(inv -> {
            final OWLClassF inst = inv.getArgument(0, OWLClassF.class);
            return Collections.singleton(new AxiomImpl<>(NamedResource.create(inst.getUri()), assertion,
                                                         new Value<>(inst.getSecondStringAttribute())));
        }).when(storageMock).getAttributeAxioms(any(), any(), any());
        when(storageMock.isInferred(any(), any())).thenReturn(true);

        assertThrows(InferredAttributeModifiedException.class, () -> uow.mergeDetached(detached, descriptor));
        verify(storageMock).isInferred(new AxiomImpl<>(NamedResource.create(original.getUri()), assertion,
                                                       new Value<>(original.getSecondStringAttribute())),
                                       Collections.singleton(CONTEXT_URI));
        verify(storageMock, never()).merge(any(), eq(metamodelMocks.forOwlClassF().stringAttribute()), any());
    }

    @Test
    void mergeDetachedIgnoresRemovalsOfInferredAttributeValuesWhenConfiguredTo() {
        uow.getConfiguration().set(JOPAPersistenceProperties.IGNORE_INFERRED_VALUE_REMOVAL_ON_MERGE, Boolean.toString(true));
        final OWLClassF original = new OWLClassF(Generators.createIndividualIdentifier());
        original.setSecondStringAttribute("Original value");
        final OWLClassF detached = new OWLClassF(original.getUri());
        detached.setSecondStringAttribute("Changed value");
        when(transactionMock.isActive()).thenReturn(true);
        when(storageMock.contains(detached.getUri(), OWLClassF.class, descriptor)).thenReturn(true);
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(original);
        defaultLoadStateDescriptor(original);
        final Assertion assertion =
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_f_stringAttribute), true);
        doAnswer(inv -> {
            final OWLClassF inst = inv.getArgument(0, OWLClassF.class);
            return Collections.singleton(new AxiomImpl<>(NamedResource.create(inst.getUri()), assertion,
                    new Value<>(inst.getSecondStringAttribute())));
        }).when(storageMock).getAttributeAxioms(any(), any(), any());
        when(storageMock.isInferred(any(), any())).thenReturn(true);

        final OWLClassF result = uow.mergeDetached(detached, descriptor);
        verify(storageMock).isInferred(new AxiomImpl<>(NamedResource.create(original.getUri()), assertion,
                        new Value<>(original.getSecondStringAttribute())),
                Collections.singleton(CONTEXT_URI));
        verify(storageMock, never()).merge(any(), eq(metamodelMocks.forOwlClassF().stringAttribute()), any());
        assertEquals(original.getSecondStringAttribute(), result.getSecondStringAttribute());
    }
}
