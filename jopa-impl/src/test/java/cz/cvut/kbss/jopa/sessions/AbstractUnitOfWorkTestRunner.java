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
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectMultilingualString;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxyGenerator;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


abstract class AbstractUnitOfWorkTestRunner extends UnitOfWorkTestBase {

    @Test
    void readObjectWithNullIdentifierArgumentThrowsNullPointerException() {
        assertThrows(NullPointerException.class, () -> uow.readObject(entityA.getClass(), null, descriptor));
        verify(serverSessionStub.getLiveObjectCache(), never()).get(any(), any(), any());
    }

    @Test
    void readObjectWithNullClassThrowsNullPointerException() {
        assertThrows(NullPointerException.class, () -> uow.readObject(null, entityB.getUri(), descriptor));
        verify(serverSessionStub.getLiveObjectCache(), never()).get(any(), any(), any());
    }

    @Test
    void readObjectWithNullDescriptorThrowsNullPointerException() {
        assertThrows(NullPointerException.class, () -> uow.readObject(entityA.getClass(), entityA.getUri(), null));
        verify(serverSessionStub.getLiveObjectCache(), never()).get(any(), any(), any());
    }

    @Test
    void readObjectLoadsObjectFromStorage() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor)))
                .thenReturn(entityA);
        defaultLoadStateDescriptor(entityA);
        OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getUri());
        verify(storageMock).find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor));
    }

    @Test
    void readAlreadyManagedObjectReturnsTheManagedOne() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertNotNull(clone);
        final OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertSame(clone, res);
        // Had to cast, otherwise ajc refused to compile this
        verify(storageMock, never()).find(any());
    }

    @Test
    void containsReturnsTrueForRegisteredExistingObject() {
        defaultLoadStateDescriptor(entityA);
        OWLClassA res = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertNotNull(res);
        assertTrue(uow.contains(res));
    }

    @Test
    void testGetState() {
        assertEquals(EntityState.NOT_MANAGED, uow.getState(entityA));
        defaultLoadStateDescriptor(entityA);
        OWLClassA toRemove = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertEquals(EntityState.MANAGED, uow.getState(toRemove));
        uow.removeObject(toRemove);
        assertEquals(EntityState.REMOVED, uow.getState(toRemove));
        final OWLClassA stateTest = Generators.generateOwlClassAInstance();
        uow.registerNewObject(stateTest, descriptor);
        assertEquals(EntityState.MANAGED_NEW, uow.getState(stateTest));
    }

    @Test
    void testGetStateWithDescriptor() {
        assertEquals(EntityState.NOT_MANAGED, uow.getState(entityA, descriptor));
        defaultLoadStateDescriptor(entityA);
        OWLClassA toRemove = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertEquals(EntityState.MANAGED, uow.getState(toRemove, descriptor));
        uow.removeObject(toRemove);
        assertEquals(EntityState.REMOVED, uow.getState(toRemove, descriptor));
        final OWLClassA stateTest = Generators.generateOwlClassAInstance();
        uow.registerNewObject(stateTest, descriptor);
        assertEquals(EntityState.MANAGED_NEW, uow.getState(stateTest, descriptor));
    }

    @Test
    void getOriginalReturnsOriginalRegisteredByReadObject() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor))).thenReturn(
                entityA);
        defaultLoadStateDescriptor(entityA);
        OWLClassA tO = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(tO);
        OWLClassA origOne = (OWLClassA) uow.getOriginal(tO);
        assertSame(entityA, origOne);
        OWLClassA origTwo = (OWLClassA) uow.getOriginal(tO);
        assertSame(origOne, origTwo);
    }

    @Test
    void getOriginalWithNullArgumentReturnsNull() {
        assertNull(uow.getOriginal(null));
    }

    @Test
    void getManagedOriginalReturnsManagedOriginalInstance() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor))).thenReturn(
                entityA);
        defaultLoadStateDescriptor(entityA);
        uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);

        final OWLClassA res = uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(entityA, res);
    }

    @Test
    void getManagedOriginalForDifferentContextReturnsNull() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor))).thenReturn(
                entityA);
        defaultLoadStateDescriptor(entityA);
        uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);

        final EntityDescriptor differentContext = new EntityDescriptor(URI.create("http://differentContext"));
        assertNull(uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), differentContext));
    }

    @Test
    void getManagedOriginalForUnknownIdentifierReturnsNull() {
        assertNull(uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor));
    }

    @Test
    void isObjectManagedReturnsTrueForRegisteredExistingObject() {
        defaultLoadStateDescriptor(entityA);
        OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertTrue(uow.isObjectManaged(managed));
    }

    @Test
    void isObjectManagedThrowsNullPointerExceptionForNullArgument() {
        assertThrows(NullPointerException.class, () -> uow.isObjectManaged(null));
    }

    @Test
    void registerExistingObjectReturnsRegisteredClone() {
        defaultLoadStateDescriptor(entityB);
        OWLClassB clone = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
        assertNotNull(clone);
        assertEquals(entityB.getUri(), clone.getUri());
        assertTrue(uow.contains(clone));
        assertSame(entityB, uow.getOriginal(clone));
    }

    /**
     * This method tests the situation when the Unit of Work has no clone to originals mapping - it was cleared. This
     * tests the second branch of the register method.
     */
    @Test
    void registerExistingObjectTwiceReturnsSameClone() {
        defaultLoadStateDescriptor(entityB);
        OWLClassB clone = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
        assertNotNull(clone);
        assertEquals(entityB.getUri(), clone.getUri());
        final OWLClassB cloneTwo = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
        assertSame(clone, cloneTwo);
    }

    @Test
    void releaseUnitOfWorkClosesStorageAndMakesUoWInactive() {
        assertTrue(uow.isActive());
        uow.release();
        assertFalse(uow.isActive());
        verify(storageMock).close();
    }

    @Test
    void unregisterRegisteredExistingObjectRemovesItFromPersistenceContext() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertTrue(uow.contains(managed));
        uow.unregisterObject(managed);
        assertFalse(uow.contains(managed));
    }

    @Test
    void commitInactiveUoWThrowsIllegalStateException() {
        uow.release();
        assertThrows(IllegalStateException.class, () -> uow.commit());
    }

    @Test
    void rollbackInactiveUoWThrowsIllegalStateException() {
        uow.release();
        assertThrows(IllegalStateException.class, () -> uow.rollback());
    }

    @Test
    void loadFieldLoadsLiteralValueAttribute() {
        final OWLClassB b = new OWLClassB();
        b.setUri(URI.create("http://bUri"));
        final Map<String, Set<String>> props = Collections
                .singletonMap(Vocabulary.p_m_IntegerSet, Collections.singleton("12345"));
        defaultLoadStateDescriptor(b);
        uow.getLoadStateRegistry().get(b)
           .setLoaded(metamodelMocks.forOwlClassB().propertiesSpec(), LoadState.NOT_LOADED);
        final OWLClassB clone = (OWLClassB) uow.registerExistingObject(b, descriptor);
        doAnswer(invocation -> {
            final FieldSpecification<?, ?> f = (FieldSpecification<?, ?>) invocation.getArguments()[1];
            EntityPropertiesUtils.setFieldValue(f.getJavaField(), invocation.getArguments()[0], props);
            return null;
        }).when(storageMock).loadFieldValue(clone, metamodelMocks.forOwlClassB().propertiesSpec(), descriptor);

        uow.loadEntityField(clone, metamodelMocks.forOwlClassB().propertiesSpec());
        assertNotNull(clone.getProperties());
        verify(storageMock).loadFieldValue(clone, metamodelMocks.forOwlClassB().propertiesSpec(), descriptor);
    }

    @Test
    void loadFieldLoadsManagedTypeAttribute() {
        final OWLClassL original = new OWLClassL(Generators.createIndividualIdentifier());
        final LoadStateDescriptor<OWLClassL> loadStateDescriptor = LoadStateDescriptorFactory.createNotLoaded(original, metamodelMocks.forOwlClassL()
                                                                                                                                      .entityType());
        uow.getLoadStateRegistry().put(original, loadStateDescriptor);
        when(metamodelMock.getLazyLoadingProxy(OWLClassA.class)).thenReturn((Class) new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class));
        final OWLClassL clone = (OWLClassL) uow.registerExistingObject(original, descriptor);
        doAnswer(invocation -> {
            final FieldSpecification<?, ?> f = (FieldSpecification<?, ?>) invocation.getArguments()[1];
            EntityPropertiesUtils.setFieldValue(f.getJavaField(), invocation.getArguments()[0], Collections.singleton(entityA));
            return null;
        }).when(storageMock)
          .loadFieldValue(eq(clone), eq(metamodelMocks.forOwlClassL().setAttribute()), eq(descriptor));
        defaultLoadStateDescriptor(entityA);

        uow.loadEntityField(clone, metamodelMocks.forOwlClassL().setAttribute());
        verify(storageMock).loadFieldValue(clone, metamodelMocks.forOwlClassL().setAttribute(), descriptor);
        assertNotNull(clone.getSet());
        assertEquals(1, clone.getSet().size());
        // Verify that the loaded value was cloned
        assertNotSame(entityA, clone.getSet().iterator().next());
        assertTrue(uow.contains(clone.getSet().iterator().next()));
    }

    @Test
    void findOfObjectAlreadyManagedAsLazilyLoadedValueReturnSameObject() {
        final OWLClassL original = new OWLClassL(Generators.createIndividualIdentifier());
        defaultLoadStateDescriptor(original);
        uow.getLoadStateRegistry().get(original)
           .setLoaded(metamodelMocks.forOwlClassL().setAttribute(), LoadState.NOT_LOADED);
        final OWLClassL clone = (OWLClassL) uow.registerExistingObject(original, descriptor);
        doAnswer(invocation -> {
            final FieldSpecification<?, ?> f = (FieldSpecification<?, ?>) invocation.getArguments()[1];
            EntityPropertiesUtils.setFieldValue(f.getJavaField(), invocation.getArguments()[0], Collections.singleton(entityA));
            return null;
        }).when(storageMock).loadFieldValue(clone, metamodelMocks.forOwlClassL().setAttribute(), descriptor);
        defaultLoadStateDescriptor(entityA);
        uow.loadEntityField(clone, metamodelMocks.forOwlClassL().setAttribute());
        assertNotNull(clone.getSet());

        final OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(clone.getSet().iterator().next(), res);
    }

    @Test
    void loadEntityFieldOnUnmanagedObjectThrowsOwlPersistenceException() {
        assertThrows(OWLPersistenceException.class, () -> uow.loadEntityField(entityB, metamodelMocks.forOwlClassB()
                                                                                                     .stringAttribute()));
        verify(storageMock, never()).loadFieldValue(any(OWLClassB.class),
                eq(metamodelMocks.forOwlClassB().stringAttribute()),
                eq(descriptor));
    }

    @Test
    void isConsistentCallsStorageConsistencyCheck() {
        when(storageMock.isConsistent(CONTEXT_URI)).thenReturn(Boolean.TRUE);
        final boolean res = uow.isConsistent(CONTEXT_URI);
        assertTrue(res);
        verify(storageMock).isConsistent(CONTEXT_URI);
    }

    @Test
    void getContextsRetrievesContextsFromStorage() {
        final List<URI> contexts = new ArrayList<>(1);
        contexts.add(CONTEXT_URI);
        when(storageMock.getContexts()).thenReturn(contexts);
        final List<URI> res = uow.getContexts();
        assertEquals(contexts, res);
        verify(storageMock).getContexts();
    }

    @Test
    void clearCleansUpPersistenceContext() {
        final OWLClassD d = new OWLClassD();
        d.setUri(URI.create("http://dUri"));
        defaultLoadStateDescriptor(d);
        uow.registerExistingObject(d, descriptor);
        final OWLClassB newOne = new OWLClassB();
        final URI pk = URI.create("http://testObject");
        newOne.setUri(pk);
        defaultLoadStateDescriptor(newOne);
        uow.registerNewObject(newOne, descriptor);
        defaultLoadStateDescriptor(entityA, entityB);
        final Object toRemove = uow.registerExistingObject(entityA, descriptor);
        uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);

        uow.clear();
        assertTrue(uow.cloneToOriginals.isEmpty());
        assertTrue(uow.keysToClones.isEmpty());
        assertTrue(uow.deletedObjects.isEmpty());
        assertTrue(uow.newObjectsCloneToOriginal.isEmpty());
        assertTrue(uow.newObjectsKeyToClone.isEmpty());
        assertFalse(uow.hasChanges());
    }

    @Test
    void unwrapReturnsItselfWhenClassMatches() {
        assertSame(uow, uow.unwrap(UnitOfWork.class));
    }

    @Test
    void rollbackDetachesAllManagedEntities() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor, false)))
                .thenReturn(entityA);
        defaultLoadStateDescriptor(entityA);
        final OWLClassA result = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        entityB.setProperties(new HashMap<>());
        uow.registerNewObject(entityB, descriptor);
        uow.rollback();
        assertFalse(uow.contains(result));
        assertFalse(uow.contains(entityB));
    }

    @Test
    void clearResetsCloneBuilder() {
        defaultLoadStateDescriptor(entityA);
        uow.registerExistingObject(entityA, descriptor);
        uow.clear();
        verify(cloneBuilder).reset();
    }

    @Test
    void registerExistingObjectInvokesPostCloneListeners() {
        final Consumer<Object> plVerifier = mock(Consumer.class);
        defaultLoadStateDescriptor(entityA);
        final Object result = uow.registerExistingObject(entityA, new CloneRegistrationDescriptor(descriptor).postCloneHandlers(List.of(plVerifier)));
        verify(plVerifier).accept(result);
    }

    @Test
    void isLoadedReturnsLoadedForRegisteredExistingObject() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(a));
    }

    @Test
    void isLoadedByAttributeReturnsLoadedForAttributesOfRegisteredExistingObject() throws Exception {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(a, OWLClassA.getStrAttField().getName()));
        assertEquals(LoadState.LOADED, uow.isLoaded(a, OWLClassA.getTypesField().getName()));
    }

    @Test
    void isLoadedReturnsUnknownForUnregisteredObject() {
        assertEquals(LoadState.UNKNOWN, uow.isLoaded(entityA));
    }

    @Test
    void isLoadedByAttributeReturnsUnknownForAttributeOfUnregisteredObject() throws Exception {
        assertEquals(LoadState.UNKNOWN, uow.isLoaded(entityA, OWLClassA.getStrAttField().getName()));
    }

    @Test
    void isLoadedByAttributeReturnsNotLoadedForNotLoadedLazilyLoadedAttribute() throws Exception {
        final LoadStateDescriptor<OWLClassL> loadStateDescriptor = LoadStateDescriptorFactory.createNotLoaded(entityL, metamodelMocks.forOwlClassL()
                                                                                                                                     .entityType());
        uow.getLoadStateRegistry().put(entityL, loadStateDescriptor);
        when(metamodelMock.getLazyLoadingProxy(OWLClassA.class)).thenReturn((Class) new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class));
        final OWLClassL instance = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        assertEquals(LoadState.NOT_LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
    }

    @Test
    void isLoadedByAttributeReturnsLoadedForNonNullValuedLazilyLoadedAttribute() throws Exception {
        defaultLoadStateDescriptor(entityL, entityA);
        entityL.setSet(Collections.singleton(entityA));
        final OWLClassL instance = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
    }

    @Test
    void loadEntityFieldCausesLoadStateOfLazilyLoadedAttributeToBeSetToLoaded() throws Exception {
        final LoadStateDescriptor<OWLClassL> loadStateDescriptor = LoadStateDescriptorFactory.createNotLoaded(entityL, metamodelMocks.forOwlClassL()
                                                                                                                                     .entityType());
        uow.getLoadStateRegistry().put(entityL, loadStateDescriptor);
        when(metamodelMock.getLazyLoadingProxy(OWLClassA.class)).thenReturn((Class) new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class));
        final OWLClassL instance = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        doAnswer(inv -> {
            final OWLClassL inst = inv.getArgument(0);
            inst.setSet(Collections.singleton(entityA));
            return null;
        }).when(storageMock).loadFieldValue(eq(instance), eq(metamodelMocks.forOwlClassL().setAttribute()), any());
        defaultLoadStateDescriptor(entityA);
        uow.loadEntityField(instance, metamodelMocks.forOwlClassL().setAttribute());

        assertEquals(LoadState.LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
    }

    @Test
    void getManagedOriginalThrowsEntityExistsExceptionWhenIndividualIsManagedAsDifferentType() {
        when(transactionMock.isActive()).thenReturn(true);
        defaultLoadStateDescriptor(entityA);
        uow.registerExistingObject(entityA, descriptor);
        assertThrows(OWLEntityExistsException.class,
                () -> uow.getManagedOriginal(OWLClassB.class, entityA.getUri(), descriptor));
    }

    @Test
    void isInferredChecksForValueInferredStatusWithConnectionWrapper() {
        defaultLoadStateDescriptor(entityD, entityA);
        final OWLClassD instance = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        uow.isInferred(instance, metamodelMocks.forOwlClassD().owlClassAAtt(), instance.getOwlClassA());
        verify(storageMock).isInferred(instance, metamodelMocks.forOwlClassD()
                                                               .owlClassAAtt(), instance.getOwlClassA(), descriptor);
    }

    @Test
    void isInferredThrowsIllegalArgumentExceptionWhenInstanceIsNotManaged() {
        assertThrows(IllegalArgumentException.class, () -> uow.isInferred(entityD, metamodelMocks.forOwlClassD()
                                                                                                 .owlClassAAtt(), entityD.getOwlClassA()));
        verify(storageMock, never()).isInferred(any(), any(), any(), any());
    }

    @Test
    void unregisterObjectReplacesPluralLazyLoadingProxiesWithEmptyCollection() {
        when(transactionMock.isActive()).thenReturn(true);
        defaultLoadStateDescriptor(entityL);
        uow.getLoadStateRegistry().get(entityL)
           .setLoaded(metamodelMocks.forOwlClassL().simpleListAtt(), LoadState.NOT_LOADED);
        uow.getLoadStateRegistry().get(entityL)
           .setLoaded(metamodelMocks.forOwlClassL().setAttribute(), LoadState.NOT_LOADED);
        final OWLClassL clone = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        // Simple list is lazy loaded
        assertInstanceOf(LazyLoadingProxy.class, clone.getSimpleList());
        assertInstanceOf(LazyLoadingProxy.class, clone.getSet());
        uow.unregisterObject(clone);
        assertNotNull(clone.getSimpleList());
        assertTrue(clone.getSimpleList().isEmpty());
        assertNotNull(clone.getSet());
        assertTrue(clone.getSet().isEmpty());
    }

    @Test
    void unregisterObjectReplacesSingularLazyLoadingProxiesWithNull() {
        when(transactionMock.isActive()).thenReturn(true);
        defaultLoadStateDescriptor(entityL);
        uow.getLoadStateRegistry().get(entityL)
           .setLoaded(metamodelMocks.forOwlClassL().owlClassAAtt(), LoadState.NOT_LOADED);
        when(metamodelMock.getLazyLoadingProxy(OWLClassA.class)).thenReturn((Class) new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class));
        final OWLClassL clone = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        assertInstanceOf(LazyLoadingProxy.class, clone.getSingleA());
        uow.unregisterObject(clone);
        assertNull(clone.getSingleA());
    }

    @Test
    void isInferredLoadsAttributeWhenItIsLazyAndNotLoaded() {
        when(transactionMock.isActive()).thenReturn(true);
        defaultLoadStateDescriptor(entityL);
        uow.getLoadStateRegistry().get(entityL)
           .setLoaded(metamodelMocks.forOwlClassL().owlClassAAtt(), LoadState.NOT_LOADED);
        when(metamodelMock.getLazyLoadingProxy(OWLClassA.class)).thenReturn((Class) new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class));
        final OWLClassL clone = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        doAnswer(inv -> {
            final OWLClassL owner = inv.getArgument(0);
            owner.setSingleA(entityA);
            return entityA;
        }).when(storageMock).loadFieldValue(clone, metamodelMocks.forOwlClassL().owlClassAAtt(), descriptor);
        assertInstanceOf(LazyLoadingProxy.class, clone.getSingleA());
        assertFalse(uow.isInferred(clone, metamodelMocks.forOwlClassL().owlClassAAtt(), clone.getSingleA()));
        assertThat(clone.getSingleA(), not(instanceOf(LazyLoadingProxy.class)));
        verify(storageMock).loadFieldValue(clone, metamodelMocks.forOwlClassL().owlClassAAtt(), descriptor);
        verify(storageMock).isInferred(clone, metamodelMocks.forOwlClassL().owlClassAAtt(), clone.getSingleA(), descriptor);
    }
}
