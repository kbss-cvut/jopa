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

import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectMultilingualString;
import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectSet;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassF;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.InferredAttributeModifiedException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxyGenerator;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ChangeTrackingUnitOfWorkTest extends ReadWriteUnitOfWorkTest {

    @BeforeEach
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected AbstractUnitOfWork initUnitOfWork() {
        return new ChangeTrackingUnitOfWork(serverSessionStub, new Configuration());
    }

    @Test
    void refreshCancelsObjectChangesInUnitOfWorkChangeSet() throws Exception {
        when(transactionMock.isActive()).thenReturn(true);
        defaultLoadStateDescriptor(entityA);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        a.setStringAttribute("updatedString");
        uow.attributeChanged(a, OWLClassA.getStrAttField());
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute(entityA.getStringAttribute());
        original.setTypes(new HashSet<>(entityA.getTypes()));
        when(storageMock.find(any())).thenReturn(original);
        defaultLoadStateDescriptor(original);
        assertNotNull(uow.uowChangeSet.getExistingObjectChanges(entityA));
        uow.refreshObject(a);
        assertNull(uow.uowChangeSet.getExistingObjectChanges(entityA));
        assertNull(uow.uowChangeSet.getExistingObjectChanges(original));
    }

    @Test
    void commitReplacesIndirectCollectionsWithRegularOnesInDetachedInstances() {
        uow.registerNewObject(entityA, descriptor);
        assertInstanceOf(ChangeTrackingIndirectSet.class, entityA.getTypes());
        uow.commit();
        assertThat(entityA.getTypes(), not(instanceOf(ChangeTrackingIndirectSet.class)));
    }

    @Test
    void detachReplacesInheritedIndirectCollectionWithRegularOne() {
        final OWLClassR entityR = new OWLClassR(Generators.createIndividualIdentifier());
        entityR.setName("test");
        final Set<String> types = Generators.generateTypes(3);
        entityR.setTypes(types);
        uow.registerNewObject(entityR, descriptor);
        assertInstanceOf(ChangeTrackingIndirectSet.class, entityR.getTypes());
        assertEquals(types, entityR.getTypes());
        uow.commit();
        assertThat(entityR.getTypes(), not(instanceOf(ChangeTrackingIndirectSet.class)));
        assertEquals(types, entityR.getTypes());
    }

    @Test
    void attributeChangedOutsideTransactionThrowsIllegalStateException() throws Exception {
        uow.clear();
        final Field strField = OWLClassA.getStrAttField();
        assertThrows(IllegalStateException.class, () -> uow.attributeChanged(entityA, strField));
        verify(storageMock, never()).merge(any(OWLClassA.class), eq(metamodelMocks.forOwlClassA().stringAttribute()),
                eq(descriptor));
    }

    @Test
    void attributeChangedMergesChangeToStorage() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        defaultLoadStateDescriptor(entityA);
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        final Field strField = OWLClassA.getStrAttField();

        uow.attributeChanged(clone, strField);
        verify(storageMock).merge(clone, metamodelMocks.forOwlClassA().stringAttribute(), descriptor);
    }

    @Test
    void attributeChangedOnUnmanagedObjectThrowsOwlPersistenceException() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final Field strField = OWLClassA.getStrAttField();
        assertThrows(OWLPersistenceException.class, () -> uow.attributeChanged(entityA, strField));
        verify(storageMock, never()).merge(any(OWLClassA.class), eq(metamodelMocks.forOwlClassA().stringAttribute()),
                eq(descriptor));
    }

    @Test
    void attributeChangedThrowsInferredAttributeModifiedExceptionOnChangeToInferredAttributeValue() {
        final OWLClassF original = new OWLClassF(Generators.createIndividualIdentifier());
        original.setSecondStringAttribute("Changed value");
        when(transactionMock.isActive()).thenReturn(true);
        defaultLoadStateDescriptor(original);
        final OWLClassF instance = (OWLClassF) uow.registerExistingObject(original, descriptor);
        // Ensure original and cloned value differ
        original.setSecondStringAttribute("Original value");
        final Assertion assertion =
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_f_stringAttribute), true);
        doAnswer(inv -> {
            final OWLClassF inst = inv.getArgument(0, OWLClassF.class);
            return Collections.singleton(new AxiomImpl<>(NamedResource.create(inst.getUri()), assertion,
                    new Value<>(inst.getSecondStringAttribute())));
        }).when(storageMock).getAttributeAxioms(any(), any(), any());
        when(storageMock.isInferred(any(), any())).thenReturn(true);


        assertThrows(InferredAttributeModifiedException.class,
                () -> uow.attributeChanged(instance, OWLClassF.getStrAttField()));
        verify(storageMock).isInferred(new AxiomImpl<>(NamedResource.create(original.getUri()), assertion,
                        new Value<>(original.getSecondStringAttribute())),
                Collections.singleton(CONTEXT_URI));
        verify(storageMock, never()).merge(any(), eq(metamodelMocks.forOwlClassF().stringAttribute()), any());
    }

    @Test
    void attributeChangedSetsAttributeLoadStatusToLoaded() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final LoadStateDescriptor<OWLClassL> loadStateDescriptor = LoadStateDescriptorFactory.createNotLoaded(entityL, metamodelMocks.forOwlClassL()
                                                                                                                                      .entityType());
        when(metamodelMock.getLazyLoadingProxy(OWLClassA.class)).thenReturn((Class) new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class));
        uow.getLoadStateRegistry().put(entityL, loadStateDescriptor);
        final OWLClassL instance = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        assertEquals(LoadState.NOT_LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
        instance.setSet(Collections.singleton(entityA));
        uow.attributeChanged(instance, OWLClassL.getSetField());

        assertEquals(LoadState.LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
    }

    @Test
    void unregisterObjectRemovesIndirectMultilingualStringOfManagedObjectBeingDetached() {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final OWLClassU entityU = new OWLClassU(Generators.createIndividualIdentifier());
        entityU.setSingularStringAtt(MultilingualString.create("test", "en"));
        defaultLoadStateDescriptor(entityU);
        final OWLClassU managed = (OWLClassU) uow.registerExistingObject(entityU, descriptor);
        assertInstanceOf(ChangeTrackingIndirectMultilingualString.class, managed.getSingularStringAtt());
        uow.unregisterObject(managed);
        assertThat(managed.getSingularStringAtt(), not(instanceOf(ChangeTrackingIndirectMultilingualString.class)));
    }

    @Test
    void removeObjectPutsExistingObjectIntoDeletedCacheAndRemovesItFromRepository() {
        defaultLoadStateDescriptor(entityB);
        final OWLClassB toRemove = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);
        assertFalse(uow.contains(toRemove));
        assertEquals(EntityState.REMOVED, uow.getState(toRemove));
        verify(storageMock).remove(entityB.getUri(), entityB.getClass(), descriptor);
    }

    @Test
    void changesToRemovedObjectAreIgnoredOnCommit() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        defaultLoadStateDescriptor(entityA);
        final OWLClassA instance = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        instance.setStringAttribute("update");
        uow.attributeChanged(instance, OWLClassA.getStrAttField());
        uow.removeObject(instance);
        assertFalse(uow.uowChangeSet.getExistingObjectsChanges().isEmpty());
        uow.commit();
        assertTrue(uow.uowChangeSet.getExistingObjectsChanges().isEmpty());
    }

    @Test
    void registerReplacesAlsoInheritedCollectionInstancesWithIndirectVersions() {
        final OWLClassR entityR = new OWLClassR(Generators.createIndividualIdentifier());
        entityR.setTypes(Generators.generateTypes(5));
        when(storageMock.find(new LoadingParameters<>(OWLClassR.class, entityR.getUri(), descriptor)))
                .thenReturn(entityR);
        defaultLoadStateDescriptor(entityR);
        final OWLClassR clone = uow.readObject(OWLClassR.class, entityR.getUri(), descriptor);
        assertInstanceOf(ChangeTrackingIndirectSet.class, clone.getTypes());
    }

    @Test
    void releaseRemovesIndirectCollectionsFromManagedEntities() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor, false)))
                .thenReturn(entityA);
        defaultLoadStateDescriptor(entityA);
        final OWLClassA result = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(result);
        assertInstanceOf(ChangeTrackingIndirectSet.class, result.getTypes());
        uow.release();
        assertThat(result.getTypes(), not(instanceOf(ChangeTrackingIndirectSet.class)));
    }

    @Test
    void loadEntityFieldDoesNotInvokeLoadFromRepositoryForNullAttributeWhenItsStateIsLoaded() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final LoadStateDescriptor<OWLClassL> loadStateDescriptor = LoadStateDescriptorFactory.createNotLoaded(entityL, metamodelMocks.forOwlClassL()
                                                                                                                                      .entityType());
        when(metamodelMock.getLazyLoadingProxy(OWLClassA.class)).thenReturn((Class) new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class));
        uow.getLoadStateRegistry().put(entityL, loadStateDescriptor);
        final OWLClassL instance = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        assertEquals(LoadState.NOT_LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
        uow.attributeChanged(instance, OWLClassL.getSetField());
        assertEquals(LoadState.LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
        uow.loadEntityField(instance, metamodelMocks.forOwlClassL().setAttribute());
        verify(storageMock, never()).loadFieldValue(eq(instance), eq(metamodelMocks.forOwlClassL().setAttribute()),
                any(Descriptor.class));
    }
}
