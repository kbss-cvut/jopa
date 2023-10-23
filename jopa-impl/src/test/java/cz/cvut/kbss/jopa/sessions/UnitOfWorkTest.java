/**
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectMap;
import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassE;
import cz.cvut.kbss.jopa.environment.OWLClassF;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exception.IdentifierNotSetException;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.EntityNotFoundException;
import cz.cvut.kbss.jopa.exceptions.InferredAttributeModifiedException;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityManagerImpl.State;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
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

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class UnitOfWorkTest extends UnitOfWorkTestBase {

    @BeforeEach
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Test
    void readObjectWithNullIdentifierArgumentThrowsNullPointerException() {
        assertThrows(NullPointerException.class, () -> uow.readObject(entityA.getClass(), null, descriptor));
        verify(cacheManagerMock, never()).get(any(), any(), any());
    }

    @Test
    void readObjectWithNullClassThrowsNullPointerException() {
        assertThrows(NullPointerException.class, () -> uow.readObject(null, entityB.getUri(), descriptor));
        verify(cacheManagerMock, never()).get(any(), any(), any());
    }

    @Test
    void readObjectWithNullDescriptorThrowsNullPointerException() {
        assertThrows(NullPointerException.class, () -> uow.readObject(entityA.getClass(), entityA.getUri(), null));
        verify(cacheManagerMock, never()).get(any(), any(), any());
    }

    @Test
    void readObjectLoadsObjectFromStorage() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor)))
                .thenReturn(entityA);
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
        verify(cacheManagerMock, times(3)).add(pks.capture(), any(Object.class), eq(descriptor));
        final Set<URI> uris = pks.getAllValues().stream().map(pk -> URI.create(pk.toString())).collect(
                Collectors.toSet());
        assertTrue(uris.contains(entityA.getUri()));
        assertTrue(uris.contains(entityB.getUri()));
        assertTrue(uris.contains(entityD.getUri()));
    }

    @Test
    void commitRemovesRemovedObjectsFromStorage() {
        final Object toRemove = uow.registerExistingObject(entityA, descriptor);
        uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);
        uow.commit();

        verify(storageMock).remove(entityA.getUri(), entityA.getClass(), descriptor);
    }

    @Test
    void commitEvictsRemovedObjectsFromCache() {
        final Object toRemove = uow.registerExistingObject(entityA, descriptor);
        uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);
        uow.commit();

        verify(cacheManagerMock).evict(OWLClassA.class, entityA.getUri(), CONTEXT_URI);
    }

    @Test
    void commitAddsNewlyAddedReferenceToObjectToCache() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final OWLClassD d = new OWLClassD();
        d.setUri(URI.create("http://tempD"));
        final OWLClassA a = new OWLClassA();
        a.setUri(URI.create("http://oldA"));
        d.setOwlClassA(a);
        final OWLClassD clone = (OWLClassD) uow.registerExistingObject(d, descriptor);
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://newA"));
        newA.setStringAttribute("somestring");
        clone.setOwlClassA(newA);
        uow.attributeChanged(clone, OWLClassD.getOwlClassAField());
        uow.registerNewObject(newA, descriptor);
        uow.commit();

        assertEquals(d.getOwlClassA().getUri(), newA.getUri());
        verify(cacheManagerMock).add(eq(newA.getUri()), any(Object.class), eq(descriptor));
    }

    @Test
    void testCalculateModificationsDataProperty() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final OWLClassA original = Generators.generateOwlClassAInstance();
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(original, descriptor);
        final String newStr = "newStr";
        clone.setStringAttribute(newStr);
        uow.attributeChanged(clone, OWLClassA.getStrAttField());
        uow.commit();

        assertEquals(newStr, original.getStringAttribute());
    }

    @Test
    void containsReturnsTrueForRegisteredExistingObject() {
        OWLClassA res = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertNotNull(res);
        assertTrue(uow.contains(res));
    }

    @Test
    void testGetState() {
        assertEquals(State.NOT_MANAGED, uow.getState(entityA));
        OWLClassA toRemove = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertEquals(State.MANAGED, uow.getState(toRemove));
        uow.removeObject(toRemove);
        assertEquals(State.REMOVED, uow.getState(toRemove));
        final OWLClassA stateTest = Generators.generateOwlClassAInstance();
        uow.registerNewObject(stateTest, descriptor);
        assertEquals(State.MANAGED_NEW, uow.getState(stateTest));
    }

    @Test
    void testGetStateWithDescriptor() {
        assertEquals(State.NOT_MANAGED, uow.getState(entityA, descriptor));
        OWLClassA toRemove = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertEquals(State.MANAGED, uow.getState(toRemove, descriptor));
        uow.removeObject(toRemove);
        assertEquals(State.REMOVED, uow.getState(toRemove, descriptor));
        final OWLClassA stateTest = Generators.generateOwlClassAInstance();
        uow.registerNewObject(stateTest, descriptor);
        assertEquals(State.MANAGED_NEW, uow.getState(stateTest, descriptor));
    }

    @Test
    void getOriginalReturnsOriginalRegisteredByReadObject() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor))).thenReturn(
                entityA);
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
        uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);

        final OWLClassA res = uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(entityA, res);
    }

    @Test
    void getManagedOriginalForDifferentContextReturnsNull() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor))).thenReturn(
                entityA);
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
        OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertFalse(uow.isObjectNew(managed));
    }

    @Test
    void isObjectManagedReturnsTrueForRegisteredExistingObject() {
        OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertTrue(uow.isObjectManaged(managed));
    }

    @Test
    void isObjectManagedThrowsNullPointerExceptionForNullArgument() {
        assertThrows(NullPointerException.class, () -> uow.isObjectManaged(null));
    }

    @Test
    void registerExistingObjectReturnsRegisteredClone() {
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
        OWLClassB clone = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
        assertNotNull(clone);
        assertEquals(entityB.getUri(), clone.getUri());
        final OWLClassB cloneTwo = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
        assertSame(clone, cloneTwo);
    }

    @Test
    void removeObjectFromCacheEvictsObjectFromCacheManager() {
        uow.removeObjectFromCache(entityB, descriptor.getSingleContext().orElse(null));
        verify(cacheManagerMock).evict(OWLClassB.class, entityB.getUri(), descriptor.getSingleContext().orElse(null));
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
        assertEquals(State.MANAGED_NEW, uow.getState(newOne));
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
    void removeObjectPutsExistingObjectIntoDeletedCacheAndRemovesItFromRepository() {
        final OWLClassB toRemove = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);
        assertFalse(uow.contains(toRemove));
        assertEquals(State.REMOVED, uow.getState(toRemove));
        verify(storageMock).remove(entityB.getUri(), entityB.getClass(), descriptor);
    }

    @Test
    void removeManagedNewObjectRemovesItFromPersistenceContext() {
        final OWLClassB newOne = new OWLClassB(Generators.createIndividualIdentifier());
        newOne.setStringAttribute("strAtt");
        this.uow.registerNewObject(newOne, descriptor);
        assertTrue(uow.contains(newOne));
        // Now try to remove it
        uow.removeObject(newOne);
        assertFalse(uow.contains(newOne));
    }

    @Test
    void removeNotRegisteredObjectThrowsIllegalArgumentException() {
        assertThrows(IllegalArgumentException.class, () -> uow.removeObject(entityA));
    }

    @Test
    void unregisterRegisteredExistingObjectRemovesItFromPersistenceContext() {
        final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertTrue(uow.contains(managed));
        uow.unregisterObject(managed);
        assertFalse(uow.contains(managed));
    }

    @Test
    void unregisterObjectRemovesItFromCloneBuilderCache() {
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
    void exceptionDuringStorageCommitRemovesUoWFromEntityManagerAndRethrowsException() {
        doThrow(OWLPersistenceException.class).when(storageMock).commit();

        assertThrows(OWLPersistenceException.class, () -> uow.commit());
        verify(emMock).removeCurrentPersistenceContext();
    }

    @Test
    void testClearCacheAfterCommit() {
        uow.registerNewObject(entityA, descriptor);
        final Object clone = uow.registerExistingObject(entityB, descriptor);
        assertTrue(uow.contains(entityA));
        assertTrue(uow.contains(clone));
        uow.setShouldClearAfterCommit(true);
        uow.commit();

        verify(cacheManagerMock).evictAll();
    }

    @Test
    void loadFieldLoadsLiteralValueAttribute() {
        final OWLClassB b = new OWLClassB();
        b.setUri(URI.create("http://bUri"));
        final Map<String, Set<String>> props = Collections
                .singletonMap(Vocabulary.p_m_IntegerSet, Collections.singleton("12345"));
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
        final OWLClassL clone = (OWLClassL) uow.registerExistingObject(original, descriptor);
        doAnswer(invocation -> {
            final FieldSpecification<?, ?> f = (FieldSpecification<?, ?>) invocation.getArguments()[1];
            EntityPropertiesUtils.setFieldValue(f.getJavaField(), invocation.getArguments()[0], Collections.singleton(entityA));
            return null;
        }).when(storageMock)
          .loadFieldValue(eq(clone), eq(metamodelMocks.forOwlClassL().setAttribute()), eq(descriptor));

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
        final OWLClassL clone = (OWLClassL) uow.registerExistingObject(original, descriptor);
        doAnswer(invocation -> {
            final FieldSpecification<?, ?> f = (FieldSpecification<?, ?>) invocation.getArguments()[1];
            EntityPropertiesUtils.setFieldValue(f.getJavaField(), invocation.getArguments()[0], Collections.singleton(entityA));
            return null;
        }).when(storageMock).loadFieldValue(clone, metamodelMocks.forOwlClassL().setAttribute(), descriptor);
        uow.loadEntityField(clone, metamodelMocks.forOwlClassL().setAttribute());
        assertNotNull(clone.getSet());

        final OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(clone.getSet().iterator().next(), res);
    }

    @Test
    void loadEntityFieldOnUnmanagedObjectThrowsOwlPersistenceException() {
        assertThrows(OWLPersistenceException.class, () -> uow.loadEntityField(entityB, metamodelMocks.forOwlClassB().stringAttribute()));
        verify(storageMock, never()).loadFieldValue(any(OWLClassB.class),
                                                    eq(metamodelMocks.forOwlClassB().stringAttribute()),
                                                    eq(descriptor));
    }

    @Test
    void attributeChangedMergesChangeToStorage() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        final Field strField = OWLClassA.getStrAttField();

        uow.attributeChanged(clone, strField);
        verify(storageMock).merge(clone, metamodelMocks.forOwlClassA().stringAttribute(), descriptor);
    }

    @Test
    void attributeMergeOnUnmanagedObjectThrowsOwlPersistenceException() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final Field strField = OWLClassA.getStrAttField();
        assertThrows(OWLPersistenceException.class, () -> uow.attributeChanged(entityA, strField));
        verify(storageMock, never()).merge(any(OWLClassA.class), eq(metamodelMocks.forOwlClassA().stringAttribute()),
                                           eq(descriptor));
    }

    @Test
    void attributeChangedOutsideTransactionThrowsIllegalStateException() throws Exception {
        final Field strField = OWLClassA.getStrAttField();
        assertThrows(IllegalStateException.class, () -> uow.attributeChanged(entityA, strField));
        verify(storageMock, never()).merge(any(OWLClassA.class), eq(metamodelMocks.forOwlClassA().stringAttribute()),
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
            lst.add(Generators.generateOwlClassAInstance());
        }
        entityL.setSimpleList(lst);
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
            lst.add(Generators.generateOwlClassAInstance());
        }
        entityL.setSimpleList(lst);
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
    void clearCleansUpPersistenceContext() throws Exception {
        final OWLClassD d = new OWLClassD();
        d.setUri(URI.create("http://dUri"));
        uow.registerExistingObject(d, descriptor);
        final OWLClassB newOne = new OWLClassB();
        final URI pk = URI.create("http://testObject");
        newOne.setUri(pk);
        uow.registerNewObject(newOne, descriptor);
        final Object toRemove = uow.registerExistingObject(entityA, descriptor);
        uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);

        uow.clear();
        assertTrue(getMap("cloneToOriginals") == null || getMap("cloneToOriginals").isEmpty());
        assertTrue(getMap("keysToClones") == null || getMap("keysToClones").isEmpty());
        assertTrue(getMap("deletedObjects") == null || getMap("deletedObjects").isEmpty());
        assertTrue(getMap("newObjectsCloneToOriginal") == null || getMap("newObjectsCloneToOriginal").isEmpty());
        assertTrue(getMap("newObjectsKeyToClone") == null || getMap("newObjectsKeyToClone").isEmpty());
        assertFalse(getBoolean("hasChanges"));
        assertFalse(getBoolean("hasNew"));
        assertFalse(getBoolean("hasDeleted"));
    }

    private Map<?, ?> getMap(String fieldName) throws Exception {
        final Field field = uow.getClass().getDeclaredField(fieldName);
        if (!field.canAccess(uow)) {
            field.setAccessible(true);
        }
        return (Map<?, ?>) field.get(uow);
    }


    private boolean getBoolean(String fieldName) throws Exception {
        final Field field = uow.getClass().getDeclaredField(fieldName);
        if (!field.canAccess(uow)) {
            field.setAccessible(true);
        }
        return (boolean) field.get(uow);
    }

    @Test
    void unwrapReturnsItselfWhenClassMatches() {
        assertSame(uow, uow.unwrap(UnitOfWork.class));
    }

    @Test
    void releaseRemovesIndirectCollectionsFromManagedEntities() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor, false)))
                .thenReturn(entityA);
        final OWLClassA result = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(result);
        assertTrue(result.getTypes() instanceof IndirectSet);
        uow.release();
        assertFalse(result.getTypes() instanceof IndirectSet);
    }

    @Test
    void rollbackDetachesAllManagedEntities() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor, false)))
                .thenReturn(entityA);
        final OWLClassA result = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        entityB.setProperties(new HashMap<>());
        uow.registerNewObject(entityB, descriptor);
        assertTrue(result.getTypes() instanceof IndirectSet);
        assertTrue(entityB.getProperties() instanceof IndirectMap);
        uow.rollback();
        assertFalse(result.getTypes() instanceof IndirectSet);
        assertFalse(entityB.getProperties() instanceof IndirectMap);
        assertFalse(uow.contains(result));
        assertFalse(uow.contains(entityB));
    }

    @Test
    void registerReplacesAlsoInheritedCollectionInstancesWithIndirectVersions() {
        final OWLClassR entityR = new OWLClassR(Generators.createIndividualIdentifier());
        entityR.setTypes(Generators.generateTypes(5));
        when(storageMock.find(new LoadingParameters<>(OWLClassR.class, entityR.getUri(), descriptor)))
                .thenReturn(entityR);
        final OWLClassR clone = uow.readObject(OWLClassR.class, entityR.getUri(), descriptor);
        assertTrue(clone.getTypes() instanceof IndirectSet);
    }

    @Test
    void commitPutsIntoCacheInstanceMergedAsDetachedDuringTransaction() {
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute("originalStringAttribute");
        when(storageMock.contains(entityA.getUri(), OWLClassA.class, descriptor)).thenReturn(true);
        when(storageMock.find(any())).thenReturn(original);

        final OWLClassA merged = uow.mergeDetached(entityA, descriptor);
        assertNotNull(merged);
        assertEquals(entityA.getStringAttribute(), merged.getStringAttribute());
        uow.commit();
        verify(cacheManagerMock).add(entityA.getUri(), original, descriptor);
    }

    @Test
    void clearResetsCloneBuilder() {
        uow.registerExistingObject(entityA, descriptor);
        uow.clear();
        verify(cloneBuilder).reset();
    }

    @Test
    void registerExistingObjectInvokesPostCloneListeners() {
        final Consumer<Object> plVerifier = mock(Consumer.class);
        final Object result = uow.registerExistingObject(entityA, descriptor, Collections.singletonList(plVerifier));
        verify(plVerifier).accept(result);
    }

    @Test
    void registerExistingObjectPassesPostCloneListenersToCloneBuilder() {
        final Consumer<Object> plVerifier = mock(Consumer.class);
        uow.registerExistingObject(entityA, descriptor, Collections.singletonList(plVerifier));
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
        final Object a = uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(a);
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class,
                                                             () -> uow.refreshObject(a));
        assertEquals("Object not managed by this persistence context.", result.getMessage());
    }

    @Test
    void refreshAcquiresNewConnectionToGetAccessToNonTransactionalEntityState() {
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        a.setStringAttribute("updatedString");
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute(entityA.getStringAttribute());
        original.setTypes(new HashSet<>(entityA.getTypes()));
        when(storageMock.find(any())).thenReturn(original);
        uow.refreshObject(a);
        // First invocation is when UoW is instantiated
        verify(serverSessionStub, times(2)).acquireConnection();
    }

    @Test
    void refreshLoadsInstanceFromRepositoryAndOverwritesFieldChanges() {
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        a.setStringAttribute("updatedString");
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute(entityA.getStringAttribute());
        original.setTypes(new HashSet<>(entityA.getTypes()));
        final LoadingParameters<OWLClassA> loadingParams =
                new LoadingParameters<>(OWLClassA.class, a.getUri(), descriptor, true);
        loadingParams.bypassCache();
        when(storageMock.find(loadingParams)).thenReturn(original);
        uow.refreshObject(a);
        assertEquals(entityA.getStringAttribute(), a.getStringAttribute());
        verify(storageMock).find(loadingParams);
    }

    @Test
    void refreshOverwritesObjectPropertyChanges() {
        final OWLClassD d = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        final OWLClassA origAClone = d.getOwlClassA();
        final OWLClassA differentA = Generators.generateOwlClassAInstance();
        final OWLClassA diffAClone = (OWLClassA) uow.registerExistingObject(differentA, descriptor);
        d.setOwlClassA(diffAClone);
        final OWLClassD original = new OWLClassD(d.getUri());
        original.setOwlClassA(entityA);
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
        final OWLClassD d = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        final OWLClassA differentA = Generators.generateOwlClassAInstance();
        d.setOwlClassA(differentA);
        final OWLClassD original = new OWLClassD(d.getUri());
        original.setOwlClassA(entityA);
        final LoadingParameters<OWLClassD> loadingParams =
                new LoadingParameters<>(OWLClassD.class, d.getUri(), descriptor, true);
        loadingParams.bypassCache();
        when(storageMock.find(loadingParams)).thenReturn(original);
        uow.refreshObject(d);

        assertEquals(original, uow.getOriginal(d));
    }

    @Test
    void refreshThrowsEntityNotFoundForNonExistentEntity() {
        final OWLClassD d = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        final LoadingParameters<OWLClassD> loadingParams =
                new LoadingParameters<>(OWLClassD.class, d.getUri(), descriptor, true);
        loadingParams.bypassCache();
        when(storageMock.find(loadingParams)).thenReturn(null);

        final EntityNotFoundException result = assertThrows(EntityNotFoundException.class, () -> uow.refreshObject(d));
        assertThat(result.getMessage(), containsString(d + " no longer exists in the repository"));
    }

    @Test
    void refreshCancelsObjectChangesInUnitOfWorkChangeSet() throws Exception {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        a.setStringAttribute("updatedString");
        uow.attributeChanged(a, OWLClassA.getStrAttField());
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute(entityA.getStringAttribute());
        original.setTypes(new HashSet<>(entityA.getTypes()));
        when(storageMock.find(any())).thenReturn(original);
        final UnitOfWorkChangeSet uowChangeSet = uow.getUowChangeSet();
        assertNotNull(uowChangeSet.getExistingObjectChanges(entityA));
        uow.refreshObject(a);
        assertNull(uowChangeSet.getExistingObjectChanges(entityA));
        assertNull(uowChangeSet.getExistingObjectChanges(original));
    }

    @Test
    void refreshOverwritesChangesSentToRepository() {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        a.setStringAttribute("updatedString");
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute(entityA.getStringAttribute());
        original.setTypes(new HashSet<>(entityA.getTypes()));
        Mockito.reset(storageMock);
        when(storageMock.find(any())).thenReturn(original);
        uow.refreshObject(a);
        verify(storageMock).merge(eq(a), eq(metamodelMocks.forOwlClassA().stringAttribute()), any(Descriptor.class));
    }

    @Test
    void restoreDeletedRegistersObjectAgain() {
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(a);

        uow.restoreRemovedObject(a);
        assertTrue(uow.contains(a));
        assertSame(entityA, uow.getOriginal(a));
    }

    @Test
    void restoreDeletedReinsertsObjectIntoRepository() {
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(a);
        uow.restoreRemovedObject(a);
        verify(storageMock).persist(a.getUri(), a, descriptor);
    }

    @Test
    void commitDetachesPersistedInstance() {
        uow.registerNewObject(entityA, descriptor);
        assertTrue(entityA.getTypes() instanceof IndirectSet);
        assertTrue(uow.contains(entityA));
        uow.commit();
        assertFalse(uow.contains(entityA));
    }

    @Test
    void commitReplacesIndirectCollectionsWithRegularOnesInDetachedInstances() {
        uow.registerNewObject(entityA, descriptor);
        assertTrue(entityA.getTypes() instanceof IndirectSet);
        uow.commit();
        assertFalse(entityA.getTypes() instanceof IndirectSet);
    }

    @Test
    void detachReplacesInheritedIndirectCollectionWithRegularOne() {
        final OWLClassR entityR = new OWLClassR(Generators.createIndividualIdentifier());
        entityR.setName("test");
        final Set<String> types = Generators.generateTypes(3);
        entityR.setTypes(types);
        uow.registerNewObject(entityR, descriptor);
        assertTrue(entityR.getTypes() instanceof IndirectSet);
        assertEquals(types, entityR.getTypes());
        uow.commit();
        assertFalse(entityR.getTypes() instanceof IndirectSet);
        assertEquals(types, entityR.getTypes());
    }

    @Test
    void commitEvictsInferredClassesFromCache() {
        uow.registerExistingObject(entityA, descriptor);
        uow.registerNewObject(entityB, descriptor);
        uow.commit();
        verify(cacheManagerMock).evictInferredObjects();
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
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(a));
    }

    @Test
    void isLoadedByAttributeReturnsLoadedForAttributesOfRegisteredExistingObject() throws Exception {
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
    void isLoadedByAttributeReturnsUnknownForNullValuedLazilyLoadedAttribute() throws Exception {
        final OWLClassL instance = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        assertEquals(LoadState.UNKNOWN, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
    }

    @Test
    void isLoadedByAttributeReturnsLoadedForNonNullValuedLazilyLoadedAttribute() throws Exception {
        entityL.setSet(Collections.singleton(entityA));
        final OWLClassL instance = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
    }

    @Test
    void loadEntityFieldCausesLoadStateOfLazilyLoadedAttributeToBeSetToLoaded() throws Exception {
        final OWLClassL instance = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        assertEquals(LoadState.UNKNOWN, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
        doAnswer(inv -> {
            final OWLClassL inst = inv.getArgument(0);
            inst.setSet(Collections.singleton(entityA));
            return null;
        }).when(storageMock).loadFieldValue(eq(instance), eq(metamodelMocks.forOwlClassL().setAttribute()), any());
        uow.loadEntityField(instance, metamodelMocks.forOwlClassL().setAttribute());

        assertEquals(LoadState.LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
    }

    @Test
    void loadEntityFieldCausesLoadStateOfLazilyLoadedAttributeToBeSetToLoadedEvenIfValueIsNull() throws Exception {
        final OWLClassL instance = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        assertEquals(LoadState.UNKNOWN, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
        // Do nothing when load field is triggered
        uow.loadEntityField(instance, metamodelMocks.forOwlClassL().setAttribute());

        assertEquals(LoadState.LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
    }

    @Test
    void attributeChangedSetsAttributeLoadStatusToLoaded() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final OWLClassL instance = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        assertEquals(LoadState.UNKNOWN, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
        instance.setSet(Collections.singleton(entityA));
        uow.attributeChanged(instance, OWLClassL.getSetField());

        assertEquals(LoadState.LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
    }

    @Test
    void loadEntityFieldDoesNotInvokeLoadFromRepositoryForNullAttributeWhenItsStateIsLoaded() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final OWLClassL instance = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        assertEquals(LoadState.UNKNOWN, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
        uow.attributeChanged(instance, OWLClassL.getSetField());
        assertEquals(LoadState.LOADED, uow.isLoaded(instance, OWLClassL.getSetField().getName()));
        uow.loadEntityField(instance, metamodelMocks.forOwlClassL().setAttribute());
        verify(storageMock, never()).loadFieldValue(eq(instance), eq(metamodelMocks.forOwlClassL().setAttribute()),
                                                    any(Descriptor.class));
    }

    @Test
    void changesToRemovedObjectAreIgnoredOnCommit() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final OWLClassA instance = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        instance.setStringAttribute("update");
        uow.attributeChanged(instance, OWLClassA.getStrAttField());
        uow.removeObject(instance);
        final UnitOfWorkChangeSet changeSet = uow.getUowChangeSet();
        assertFalse(changeSet.getExistingObjectsChanges().isEmpty());
        uow.commit();
        assertTrue(changeSet.getExistingObjectsChanges().isEmpty());
    }

    @Test
    void getManagedOriginalThrowsEntityExistsExceptionWhenIndividualIsManagedAsDifferentType() {
        when(transactionMock.isActive()).thenReturn(true);
        uow.registerExistingObject(entityA, descriptor);
        assertThrows(OWLEntityExistsException.class,
                     () -> uow.getManagedOriginal(OWLClassB.class, entityA.getUri(), descriptor));
    }

    @Test
    void getManagedOriginalReturnsNullWhenObjectIsManagedButAmongDeletedObjects() {
        when(transactionMock.isActive()).thenReturn(true);
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
    void attributeChangedThrowsInferredAttributeModifiedExceptionOnChangeToInferredAttributeValue() {
        final OWLClassF original = new OWLClassF(Generators.createIndividualIdentifier());
        original.setSecondStringAttribute("Changed value");
        when(transactionMock.isActive()).thenReturn(true);
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
    void isInferredChecksForValueInferredStatusWithConnectionWrapper() {
        final OWLClassD instance = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        uow.isInferred(instance, metamodelMocks.forOwlClassD().owlClassAAtt(), instance.getOwlClassA());
        verify(storageMock).isInferred(instance, metamodelMocks.forOwlClassD().owlClassAAtt(), instance.getOwlClassA(), descriptor);
    }

    @Test
    void isInferredThrowsIllegalArgumentExceptionWhenInstanceIsNotManaged() {
        assertThrows(IllegalArgumentException.class, () -> uow.isInferred(entityD, metamodelMocks.forOwlClassD().owlClassAAtt(), entityD.getOwlClassA()));
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
}
