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
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassE;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exception.IdentifierNotSetException;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.EntityNotFoundException;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxyGenerator;
import cz.cvut.kbss.jopa.sessions.cache.Descriptors;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import java.lang.annotation.Annotation;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static cz.cvut.kbss.jopa.environment.utils.ContainsSameEntities.containsSameEntities;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
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
import static org.mockito.Mockito.times;
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
    void readNewlyRegisteredObjectReturnsIt() {
        uow.registerNewObject(entityA, descriptor);
        assertTrue(uow.contains(entityA));
        final OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertSame(entityA, res);
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
    void commitAddsNewObjectsToCache() {
        uow.registerNewObject(entityA, descriptor);
        uow.registerNewObject(entityB, descriptor);
        uow.registerNewObject(entityD, descriptor);
        uow.commit();

        ArgumentCaptor<Object> pks = ArgumentCaptor.forClass(Object.class);
        verify(serverSessionStub.getLiveObjectCache(), times(3)).add(pks.capture(), any(Object.class), any(Descriptors.class));
        final Set<URI> uris = pks.getAllValues().stream().map(pk -> URI.create(pk.toString())).collect(
                Collectors.toSet());
        assertTrue(uris.contains(entityA.getUri()));
        assertTrue(uris.contains(entityB.getUri()));
        assertTrue(uris.contains(entityD.getUri()));
    }

    @Test
    void commitRemovesRemovedObjectsFromStorage() {
        defaultLoadStateDescriptor(entityA, entityB);
        final Object toRemove = uow.registerExistingObject(entityA, descriptor);
        uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);
        uow.commit();

        verify(storageMock).remove(entityA.getUri(), entityA.getClass(), descriptor);
    }

    @Test
    void commitEvictsRemovedObjectsFromCache() {
        defaultLoadStateDescriptor(entityA, entityB);
        final Object toRemove = uow.registerExistingObject(entityA, descriptor);
        uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);
        uow.commit();

        verify(serverSessionStub.getLiveObjectCache()).evict(OWLClassA.class, entityA.getUri(), CONTEXT_URI);
    }

    @Test
    void commitAddsNewlyAddedReferenceToObjectToCache() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final OWLClassD d = new OWLClassD();
        d.setUri(URI.create("http://tempD"));
        final OWLClassA a = new OWLClassA();
        a.setUri(URI.create("http://oldA"));
        d.setOwlClassA(a);
        defaultLoadStateDescriptor(d);
        defaultLoadStateDescriptor(a);
        final OWLClassD clone = (OWLClassD) uow.registerExistingObject(d, descriptor);
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://newA"));
        newA.setStringAttribute("somestring");
        clone.setOwlClassA(newA);
        uow.attributeChanged(clone, OWLClassD.getOwlClassAField());
        uow.registerNewObject(newA, descriptor);
        uow.commit();

        assertEquals(d.getOwlClassA().getUri(), newA.getUri());
        verify(serverSessionStub.getLiveObjectCache()).add(eq(newA.getUri()), any(Object.class), any(Descriptors.class));
    }

    @Test
    void testCalculateModificationsDataProperty() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final OWLClassA original = Generators.generateOwlClassAInstance();
        defaultLoadStateDescriptor(original);
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(original, descriptor);
        final String newStr = "newStr";
        clone.setStringAttribute(newStr);
        uow.attributeChanged(clone, OWLClassA.getStrAttField());
        uow.commit();

        assertEquals(newStr, original.getStringAttribute());
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
    void isObjectNewReturnsTrueForNewlyRegisteredObject() {
        final OWLClassA testNew = Generators.generateOwlClassAInstance();
        uow.registerNewObject(testNew, descriptor);
        assertTrue(uow.isObjectNew(testNew));
    }

    @Test
    void isObjectNewReturnsFalseForNullArgument() {
        assertFalse(uow.isObjectNew(null));
    }

    @Test
    void isObjectNewReturnsFalseForRegisteredExistingObject() {
        defaultLoadStateDescriptor(entityA);
        OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertFalse(uow.isObjectNew(managed));
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
    void removeObjectFromCacheEvictsObjectFromCacheManager() {
        uow.removeObjectFromCache(entityB, descriptor.getSingleContext().orElse(null));
        verify(serverSessionStub.getLiveObjectCache()).evict(OWLClassB.class, entityB.getUri(), descriptor.getSingleContext()
                                                                                                          .orElse(null));
    }

    @Test
    void registerNewObjectGeneratesIdentifierWhenInstancesDoesNotHaveOne() {
        final OWLClassE entity = new OWLClassE();
        entity.setStringAttribute("Test value");
        final URI id = Generators.createIndividualIdentifier();
        when(storageMock.generateIdentifier(any(EntityType.class))).thenReturn(id);
        uow.registerNewObject(entity, descriptor);
        assertEquals(id, entity.getUri());
        verify(storageMock).generateIdentifier(metamodelMocks.forOwlClassE().entityType());
    }

    @Test
    void registerNewObjectAddsArgumentToPersistenceContext() {
        final OWLClassA newOne = Generators.generateOwlClassAInstance();
        uow.registerNewObject(newOne, descriptor);
        assertTrue(uow.contains(newOne));
        assertEquals(EntityState.MANAGED_NEW, uow.getState(newOne));
        verify(storageMock, never()).persist(newOne.getUri(), newOne, descriptor);
    }

    @Test
    void registerNewObjectThrowsNullPointerExceptionForNullArgument() {
        assertThrows(NullPointerException.class, () -> uow.registerNewObject(null, descriptor));
    }

    @Test
    void registerNewObjectThrowsNullPointerExceptionForNullDescriptor() {
        assertThrows(NullPointerException.class, () -> uow.registerNewObject(entityA, null));
    }

    @Test
    void registerNewObjectThrowsIdentifierNotSetExceptionWhenIdentifierIsNullAndNotGenerated() {
        final OWLClassB b = new OWLClassB();
        assertThrows(IdentifierNotSetException.class, () -> uow.registerNewObject(b, descriptor));
        verify(storageMock, never()).persist(any(Object.class), any(Object.class), eq(descriptor));
    }

    @Test
    void releaseUnitOfWorkClosesStorageAndMakesUoWInactive() {
        assertTrue(uow.isActive());
        uow.release();
        assertFalse(uow.isActive());
        verify(storageMock).close();
    }

    @Test
    void removeObjectRemovesNewlyRegisteredObjectFromPersistenceContext() {
        final OWLClassB newOne = new OWLClassB(Generators.createIndividualIdentifier());
        newOne.setStringAttribute("strAtt");
        uow.registerNewObject(newOne, descriptor);
        assertTrue(uow.contains(newOne));

        uow.removeObject(newOne);
        assertFalse(uow.contains(newOne));
    }

    @Test
    void removeNotRegisteredObjectThrowsIllegalArgumentException() {
        assertThrows(IllegalArgumentException.class, () -> uow.removeObject(entityA));
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
    void unregisterObjectRemovesItFromCloneBuilderCache() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.unregisterObject(managed);
        verify(cloneBuilder).removeVisited(entityA, descriptor);
    }

    @Test
    void commitInactiveUoWThrowsIllegalStateException() {
        uow.release();
        assertThrows(IllegalStateException.class, () -> uow.commit());
    }

    @Test
    void rollbackRollsBackStorageChangesAndRemovesObjectsFromPersistenceContext() {
        uow.registerNewObject(entityA, descriptor);
        defaultLoadStateDescriptor(entityB);
        final Object clone = uow.registerExistingObject(entityB, descriptor);
        assertTrue(uow.contains(entityA));
        assertTrue(uow.contains(clone));

        uow.rollback();
        verify(storageMock).rollback();
        assertFalse(uow.contains(entityA));
        assertFalse(uow.contains(clone));
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
    void throwsCardinalityViolationWhenMaximumCardinalityIsViolatedOnCommit() {
        final List<OWLClassA> lst = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            lst.add(Generators.generateOwlClassAInstance());
        }
        entityL.setReferencedList(lst);
        uow.registerNewObject(entityL, descriptor);
        assertThrows(CardinalityConstraintViolatedException.class, () -> uow.commit());
        verify(storageMock, never()).commit();
    }

    @Test
    void throwsCardinalityViolationExceptionWhenMinimumCardinalityIsViolatedOnCommit() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final List<OWLClassA> lst = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            final OWLClassA a = Generators.generateOwlClassAInstance();
            lst.add(a);
            defaultLoadStateDescriptor(a);
        }
        entityL.setSimpleList(lst);
        defaultLoadStateDescriptor(entityL);
        final OWLClassL clone = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        clone.getSimpleList().clear();
        uow.attributeChanged(clone, OWLClassL.getSimpleListField());
        assertThrows(CardinalityConstraintViolatedException.class, () -> uow.commit());
        verify(storageMock, never()).commit();
    }

    @Test
    void icValidationPassesOnCommitWhenConstraintsAreViolatedAndThenFixedDuringTransaction() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final List<OWLClassA> lst = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            final OWLClassA a = Generators.generateOwlClassAInstance();
            lst.add(a);
            defaultLoadStateDescriptor(a);
        }
        entityL.setSimpleList(lst);
        defaultLoadStateDescriptor(entityL);
        final OWLClassL clone = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        clone.setSimpleList(Collections.emptyList());
        uow.attributeChanged(clone, OWLClassL.getSimpleListField());
        final List<OWLClassA> updatedList = new ArrayList<>();
        for (int i = 100; i < 103; i++) {
            updatedList.add(Generators.generateOwlClassAInstance());
        }
        clone.setSimpleList(updatedList);
        uow.attributeChanged(clone, OWLClassL.getSimpleListField());
        uow.commit();
        verify(storageMock).commit();
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
    void commitPutsIntoCacheInstanceMergedAsDetachedDuringTransaction() {
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute("originalStringAttribute");
        when(storageMock.contains(entityA.getUri(), OWLClassA.class, descriptor)).thenReturn(true);
        when(storageMock.find(any())).thenReturn(original);
        final LoadStateDescriptor<OWLClassA> loadStateDescriptor = LoadStateDescriptorFactory.createAllLoaded(original, metamodelMocks.forOwlClassA()
                                                                                                                                      .entityType());
        uow.loadStateRegistry.put(original, loadStateDescriptor);

        final OWLClassA merged = uow.mergeDetached(entityA, descriptor);
        assertNotNull(merged);
        assertEquals(entityA.getStringAttribute(), merged.getStringAttribute());
        uow.commit();
        verify(serverSessionStub.getLiveObjectCache()).add(entityA.getUri(), original, new Descriptors(descriptor, loadStateDescriptor));
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
    void registerExistingObjectPassesPostCloneListenersToCloneBuilder() {
        defaultLoadStateDescriptor(entityA);
        final Consumer<Object> plVerifier = mock(Consumer.class);
        uow.registerExistingObject(entityA, new CloneRegistrationDescriptor(descriptor).postCloneHandlers(List.of(plVerifier)));
        final ArgumentCaptor<CloneConfiguration> captor = ArgumentCaptor.forClass(CloneConfiguration.class);
        verify(cloneBuilder).buildClone(eq(entityA), captor.capture());
        assertTrue(captor.getValue().getPostRegister().contains(plVerifier));
    }

    @Test
    void refreshThrowsIllegalArgumentForNonManagedInstance() {
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class,
                () -> uow.refreshObject(
                        Generators.generateOwlClassAInstance()));
        assertEquals("Object not managed by this persistence context.", result.getMessage());
    }

    @Test
    void refreshThrowsIllegalArgumentForRemovedInstance() {
        defaultLoadStateDescriptor(entityA);
        final Object a = uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(a);
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class,
                () -> uow.refreshObject(a));
        assertEquals("Object not managed by this persistence context.", result.getMessage());
    }

    @Test
    void refreshAcquiresNewConnectionToGetAccessToNonTransactionalEntityState() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        a.setStringAttribute("updatedString");
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute(entityA.getStringAttribute());
        original.setTypes(new HashSet<>(entityA.getTypes()));
        when(storageMock.find(any())).thenReturn(original);
        defaultLoadStateDescriptor(original);
        uow.refreshObject(a);
        // First invocation is when UoW is instantiated
        verify(serverSessionStub, times(2)).acquireConnection();
    }

    @Test
    void refreshLoadsInstanceFromRepositoryAndOverwritesFieldChanges() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        a.setStringAttribute("updatedString");
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute(entityA.getStringAttribute());
        original.setTypes(new HashSet<>(entityA.getTypes()));
        final LoadingParameters<OWLClassA> loadingParams =
                new LoadingParameters<>(OWLClassA.class, a.getUri(), descriptor, true);
        loadingParams.bypassCache();
        defaultLoadStateDescriptor(original);
        when(storageMock.find(loadingParams)).thenReturn(original);
        uow.refreshObject(a);
        assertEquals(entityA.getStringAttribute(), a.getStringAttribute());
        verify(storageMock).find(loadingParams);
    }

    @Test
    void refreshOverwritesObjectPropertyChanges() {
        defaultLoadStateDescriptor(entityD, entityA);
        final OWLClassD d = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        final OWLClassA origAClone = d.getOwlClassA();
        final OWLClassA differentA = Generators.generateOwlClassAInstance();
        defaultLoadStateDescriptor(differentA);
        final OWLClassA diffAClone = (OWLClassA) uow.registerExistingObject(differentA, descriptor);
        d.setOwlClassA(diffAClone);
        final OWLClassD original = new OWLClassD(d.getUri());
        original.setOwlClassA(entityA);
        defaultLoadStateDescriptor(original);
        final LoadingParameters<OWLClassD> loadingParams =
                new LoadingParameters<>(OWLClassD.class, d.getUri(), descriptor, true);
        loadingParams.bypassCache();
        when(storageMock.find(loadingParams)).thenReturn(original);

        uow.refreshObject(d);
        assertNotEquals(diffAClone, d.getOwlClassA());
        assertNotSame(entityA, d.getOwlClassA());
        assertEquals(origAClone.getUri(), d.getOwlClassA().getUri());
    }

    @Test
    void refreshSetsUpdatesCloneMappingForRefreshedInstance() {
        defaultLoadStateDescriptor(entityD, entityA);
        final OWLClassD d = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        final OWLClassA differentA = Generators.generateOwlClassAInstance();
        d.setOwlClassA(differentA);
        final OWLClassD original = new OWLClassD(d.getUri());
        original.setOwlClassA(entityA);
        final LoadingParameters<OWLClassD> loadingParams =
                new LoadingParameters<>(OWLClassD.class, d.getUri(), descriptor, true);
        loadingParams.bypassCache();
        when(storageMock.find(loadingParams)).thenReturn(original);
        defaultLoadStateDescriptor(original);
        uow.refreshObject(d);

        assertEquals(original, uow.getOriginal(d));
    }

    @Test
    void refreshThrowsEntityNotFoundForNonExistentEntity() {
        defaultLoadStateDescriptor(entityD, entityA);
        final OWLClassD d = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        final LoadingParameters<OWLClassD> loadingParams =
                new LoadingParameters<>(OWLClassD.class, d.getUri(), descriptor, true);
        loadingParams.bypassCache();
        when(storageMock.find(loadingParams)).thenReturn(null);

        final EntityNotFoundException result = assertThrows(EntityNotFoundException.class, () -> uow.refreshObject(d));
        assertThat(result.getMessage(), containsString(d + " no longer exists in the repository"));
    }

    @Test
    void refreshOverwritesChangesSentToRepository() {
        when(transactionMock.isActive()).thenReturn(true);
        defaultLoadStateDescriptor(entityA);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        a.setStringAttribute("updatedString");
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute(entityA.getStringAttribute());
        original.setTypes(new HashSet<>(entityA.getTypes()));
        Mockito.reset(storageMock);
        when(storageMock.find(any())).thenReturn(original);
        defaultLoadStateDescriptor(original);
        uow.refreshObject(a);
        verify(storageMock).merge(eq(a), eq(metamodelMocks.forOwlClassA().stringAttribute()), any(Descriptor.class));
    }

    @Test
    void restoreDeletedRegistersObjectAgain() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(a);

        uow.restoreRemovedObject(a);
        assertTrue(uow.contains(a));
        assertSame(entityA, uow.getOriginal(a));
    }

    @Test
    void restoreDeletedReinsertsObjectIntoRepository() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(a);
        uow.restoreRemovedObject(a);
        verify(storageMock).persist(a.getUri(), a, descriptor);
    }

    @Test
    void commitDetachesPersistedInstance() {
        uow.registerNewObject(entityA, descriptor);
        assertTrue(uow.contains(entityA));
        uow.commit();
        assertFalse(uow.contains(entityA));
    }

    @Test
    void commitEvictsInferredClassesFromCache() {
        defaultLoadStateDescriptor(entityA);
        uow.registerExistingObject(entityA, descriptor);
        uow.registerNewObject(entityB, descriptor);
        uow.commit();
        verify(serverSessionStub.getLiveObjectCache()).evictInferredObjects();
    }

    @Test
    void isLoadedReturnsLoadedForNewlyRegisteredInstance() {
        uow.registerNewObject(entityA, descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(entityA));
    }

    @Test
    void isLoadedByAttributeReturnsLoadedForAttributesOfNewlyRegisteredInstance() throws Exception {
        uow.registerNewObject(entityA, descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(entityA, OWLClassA.getStrAttField().getName()));
        assertEquals(LoadState.LOADED, uow.isLoaded(entityA, OWLClassA.getTypesField().getName()));
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
    void getManagedOriginalReturnsNullWhenObjectIsManagedButAmongDeletedObjects() {
        when(transactionMock.isActive()).thenReturn(true);
        defaultLoadStateDescriptor(entityA);
        final Object entity = uow.registerExistingObject(entityA, descriptor);
        assertNotNull(uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor));
        uow.removeObject(entity);
        assertNull(uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor));
    }

    /**
     * Enhancement #98
     */
    @Test
    void persistSkipsCardinalityConstraintValidationOfInferredAttributes() {
        when(transactionMock.isActive()).thenReturn(true);
        final Attribute<OWLClassA, String> strAtt = metamodelMocks.forOwlClassA().stringAttribute();
        when(strAtt.isInferred()).thenReturn(true);
        final ParticipationConstraint pc = new ParticipationConstraint() {

            @Override
            public Class<? extends Annotation> annotationType() {
                return ParticipationConstraint.class;
            }

            @Override
            public String owlObjectIRI() {
                return Vocabulary.p_a_stringAttribute;
            }

            @Override
            public int min() {
                return 1;
            }

            @Override
            public int max() {
                return 1;
            }
        };
        when(strAtt.getConstraints()).thenReturn(new ParticipationConstraint[]{pc});
        entityA.setStringAttribute(null);
        uow.registerNewObject(entityA, descriptor);
        assertDoesNotThrow(() -> uow.commit());
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
    void commitPersistsAllNewlyRegisteredObjects() {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        uow.registerNewObject(entityA, descriptor);
        uow.registerNewObject(entityB, descriptor);
        verify(storageMock, never()).persist(any(), any(), any());
        uow.commit();

        verify(storageMock).persist(entityA.getUri(), entityA, descriptor);
        verify(storageMock).persist(entityB.getUri(), entityB, descriptor);
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
    void getInstanceForMergeTriggersLazyAttributeLoadingForAlreadyManagedObject() {
        when(transactionMock.isActive()).thenReturn(true);
        final List<OWLClassA> simpleList = Generators.generateInstances(2);
        defaultLoadStateDescriptor(simpleList.toArray());
        final OWLClassC entityC = new OWLClassC(Generators.createIndividualIdentifier());
        defaultLoadStateDescriptor(entityC);
        uow.getLoadStateRegistry().get(entityC)
           .setLoaded(metamodelMocks.forOwlClassC().simpleListAtt(), LoadState.NOT_LOADED);
        final OWLClassC clone = (OWLClassC) uow.registerExistingObject(entityC, descriptor);
        assertInstanceOf(LazyLoadingProxy.class, clone.getSimpleList());
        doAnswer(inv -> {
            final OWLClassC instance = inv.getArgument(0);
            instance.setSimpleList(simpleList);
            return simpleList;
        }).when(storageMock).loadFieldValue(clone, metamodelMocks.forOwlClassC().simpleListAtt(), descriptor);

        final OWLClassC result = uow.getInstanceForMerge(entityC.getUri(), metamodelMocks.forOwlClassC()
                                                                                         .entityType(), descriptor);
        assertThat(result.getSimpleList(), containsSameEntities(simpleList));
    }
}
