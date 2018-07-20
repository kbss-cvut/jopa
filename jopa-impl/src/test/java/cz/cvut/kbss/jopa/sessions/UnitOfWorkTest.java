/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.EntityNotFoundException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityManagerImpl.State;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.ontodriver.exception.PrimaryKeyNotSetException;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class UnitOfWorkTest extends UnitOfWorkTestBase {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Before
    public void setUp() throws Exception {
        super.setUp();
    }

    @Test(expected = NullPointerException.class)
    public void testReadObjectNullPrimaryKey() {
        try {
            uow.readObject(entityA.getClass(), null, descriptor);
        } finally {
            verify(cacheManagerMock, never()).get(any(), any(), any());
        }
    }

    @Test(expected = NullPointerException.class)
    public void testReadObjectNullClass() {
        try {
            uow.readObject(null, entityB.getUri(), descriptor);
        } finally {
            verify(cacheManagerMock, never()).get(any(), any(), any());
        }
    }

    @Test(expected = NullPointerException.class)
    public void testReadObjectNullContext() {
        try {
            uow.readObject(entityA.getClass(), entityA.getUri(), null);
        } finally {
            verify(cacheManagerMock, never()).get(any(), any(), any());
        }
    }

    @Test
    public void testReadObjectFromOntology() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor)))
                .thenReturn(entityA);
        OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getUri());
    }

    @Test
    public void testReadObjectJustPersisted() {
        uow.registerNewObject(entityA, descriptor);
        assertTrue(uow.contains(entityA));
        final OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(entityA, res);
    }

    @Test
    public void readAlreadyManagedObjectReturnsTheManagedOne() {
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertNotNull(clone);
        final OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertSame(clone, res);
        // Had to cast, otherwise ajc refused to compile this
        verify(storageMock, never()).find(any());
    }

    @Test
    public void testCalculateNewObjects() {
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
    public void testCalculateDeletedObjects() {
        final Object toRemove = uow.registerExistingObject(entityA, descriptor);
        uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);
        uow.commit();

        verify(cacheManagerMock).evict(OWLClassA.class, entityA.getUri(), CONTEXT_URI);
        verify(storageMock).remove(entityA.getUri(), entityA.getClass(), descriptor);
    }

    @Test
    public void testCalculateModificationsObjectProperty() throws Exception {
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
    public void testCalculateModificationsDataProperty() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final OWLClassA newA = new OWLClassA();
        newA.setUri(URI.create("http://newA"));
        newA.setStringAttribute("somestring");
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(newA, descriptor);
        // Trigger change, otherwise we would have to stub
        // OWLAPIPersistenceProvider's emfs and server session
        uow.setHasChanges();
        final String newStr = "newStr";
        clone.setStringAttribute(newStr);
        uow.attributeChanged(clone, OWLClassA.getStrAttField());
        uow.commit();

        assertEquals(newStr, newA.getStringAttribute());
    }

    @Test
    public void testContains() {
        OWLClassA res = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertNotNull(res);
        assertTrue(uow.contains(res));
    }

    @Test
    public void testGetState() {
        assertEquals(State.NOT_MANAGED, uow.getState(entityA));
        OWLClassA toRemove = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertEquals(State.MANAGED, uow.getState(toRemove));
        uow.removeObject(toRemove);
        assertEquals(State.REMOVED, uow.getState(toRemove));
        final OWLClassA stateTest = new OWLClassA();
        final URI pk = URI.create("http://stateTest");
        stateTest.setUri(pk);
        uow.registerNewObject(stateTest, descriptor);
        assertEquals(State.MANAGED_NEW, uow.getState(stateTest));
    }

    @Test
    public void testGetStateWithDescriptor() {
        assertEquals(State.NOT_MANAGED, uow.getState(entityA, descriptor));
        OWLClassA toRemove = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertEquals(State.MANAGED, uow.getState(toRemove, descriptor));
        uow.removeObject(toRemove);
        assertEquals(State.REMOVED, uow.getState(toRemove, descriptor));
        final OWLClassA stateTest = new OWLClassA();
        final URI pk = URI.create("http://stateTest");
        stateTest.setUri(pk);
        uow.registerNewObject(stateTest, descriptor);
        assertEquals(State.MANAGED_NEW, uow.getState(stateTest, descriptor));
    }

    @Test
    public void testGetOriginal() {
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
    public void testGetOriginalNull() {
        assertNull(uow.getOriginal(null));
    }

    @Test
    public void getManagedOriginalReturnsManagedOriginalInstance() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor))).thenReturn(
                entityA);
        uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);

        final OWLClassA res = uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(entityA, res);
    }

    @Test
    public void getManagedOriginalForDifferentContextReturnsNull() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor))).thenReturn(
                entityA);
        uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);

        final EntityDescriptor differentContext = new EntityDescriptor(URI.create("http://differentContext"));
        assertNull(uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), differentContext));
    }

    @Test
    public void getManagedOriginalForUnknownIdentifierReturnsNull() {
        assertNull(uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor));
    }

    @Test
    public void testIsObjectNew() {
        final OWLClassA testNew = new OWLClassA();
        final URI pk = URI.create("http://testNewOne");
        testNew.setUri(pk);
        uow.registerNewObject(testNew, descriptor);
        assertTrue(uow.isObjectNew(testNew));
        verify(storageMock).persist(pk, testNew, descriptor);
    }

    @Test
    public void testIsObjectNewWithNullAndManaged() {
        assertFalse(uow.isObjectNew(null));
        OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertFalse(uow.isObjectNew(managed));
    }

    @Test
    public void testIsObjectManaged() {
        OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertTrue(uow.isObjectManaged(managed));
    }

    @Test(expected = NullPointerException.class)
    public void testIsObjectManagerNull() {
        uow.isObjectManaged(null);
        fail("This line should not have been reached.");
    }

    @Test
    public void testRegisterExistingObject() {
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
    public void testRegisterExistingObjectTwice() {
        OWLClassB clone = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
        assertNotNull(clone);
        assertEquals(entityB.getUri(), clone.getUri());
        final OWLClassB cloneTwo = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
        assertSame(clone, cloneTwo);
    }

    @Test
    public void testRemoveObjectFromCache() {
        uow.removeObjectFromCache(entityB, descriptor.getContext());
        verify(cacheManagerMock).evict(OWLClassB.class, entityB.getUri(),
                descriptor.getContext());
    }

    @Test
    public void testRegisterNewObject() {
        final OWLClassA newOne = new OWLClassA();
        final URI pk = URI.create("http://newEntity");
        newOne.setUri(pk);
        newOne.setStringAttribute("stringAttributeOne");
        uow.registerNewObject(newOne, descriptor);
        assertTrue(uow.getNewObjectsCloneToOriginal().containsKey(newOne));
        verify(storageMock).persist(pk, newOne, descriptor);
    }

    @Test(expected = NullPointerException.class)
    public void testRegisterNewObjectNull() {
        uow.registerNewObject(null, descriptor);
        fail("This line should not have been reached.");
    }

    @Test(expected = NullPointerException.class)
    public void testRegisterNewObjectNullDescriptor() {
        uow.registerNewObject(entityA, null);
        fail("This line should not have been reached.");
    }

    @Test(expected = PrimaryKeyNotSetException.class)
    public void testRegisterNewObjectNullPkNotGenerated() {
        final OWLClassB b = new OWLClassB();
        try {
            uow.registerNewObject(b, descriptor);
        } finally {
            verify(storageMock, never()).persist(any(Object.class), any(Object.class),
                    eq(descriptor));
        }
        fail("This line should not have been reached.");
    }

    @Test
    public void testReleaseUnitOfWork() {
        assertTrue(uow.isActive());
        uow.release();
        assertFalse(uow.isActive());
        verify(storageMock).close();
    }

    @Test
    public void testRemoveObject() {
        final OWLClassB toRemove = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);
        assertTrue(uow.getDeletedObjects().containsKey(toRemove));
        assertFalse(uow.contains(toRemove));
        verify(storageMock).remove(entityB.getUri(), entityB.getClass(), descriptor);
    }

    @Test
    public void testRemoveNewObject() {
        final OWLClassB newOne = new OWLClassB();
        final URI pk = URI.create("http://testObject");
        newOne.setUri(pk);
        newOne.setStringAttribute("strAtt");
        this.uow.registerNewObject(newOne, descriptor);
        assertTrue(uow.contains(newOne));
        // Now try to remove it
        uow.removeObject(newOne);
        assertFalse(uow.contains(newOne));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRemoveObjectNotRegistered() {
        uow.removeObject(entityA);
    }

    @Test
    public void testUnregisterObject() {
        final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertTrue(uow.contains(managed));
        uow.unregisterObject(managed);
        assertFalse(uow.contains(managed));
    }

    @Test
    public void unregisterObjectRemovesItFromCloneBuilderCache() {
        final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.unregisterObject(managed);
        verify(cloneBuilder).removeVisited(entityA, descriptor);
    }

    @Test
    public void testUseTransactionalOntologyForQueryProcessing() {
        assertTrue(uow.useTransactionalOntologyForQueryProcessing());
        assertFalse(uow.useBackupOntologyForQueryProcessing());
    }

    @Test
    public void testSwitchQueryProcessingOntology() {
        uow.setUseBackupOntologyForQueryProcessing();
        assertTrue(uow.useBackupOntologyForQueryProcessing());
        assertFalse(uow.useTransactionalOntologyForQueryProcessing());
        uow.setUseTransactionalOntologyForQueryProcessing();
        assertTrue(uow.useTransactionalOntologyForQueryProcessing());
        assertFalse(uow.useBackupOntologyForQueryProcessing());
    }

    @Test(expected = IllegalStateException.class)
    public void testCommitInactive() {
        uow.release();
        uow.commit();
        fail("This line should not have been reached.");
    }

    @Test
    public void testRollback() {
        uow.registerNewObject(entityA, descriptor);
        final Object clone = uow.registerExistingObject(entityB, descriptor);
        verify(storageMock).persist(entityA.getUri(), entityA, descriptor);
        assertTrue(uow.contains(entityA));
        assertTrue(uow.contains(clone));

        uow.rollback();
        verify(storageMock).rollback();
        assertFalse(uow.contains(entityA));
        assertFalse(uow.contains(clone));
    }

    @Test(expected = IllegalStateException.class)
    public void testRollbackInactive() {
        uow.release();
        uow.rollback();
        fail("This line should not have been reached.");
    }

    @Test(expected = OWLPersistenceException.class)
    public void testCommitFailed() {
        doThrow(OWLPersistenceException.class).when(storageMock).commit();
        try {
            uow.commit();
        } finally {
            verify(emMock).removeCurrentPersistenceContext();
        }
    }

    @Test
    public void testClearCacheAfterCommit() {
        uow.registerNewObject(entityA, descriptor);
        final Object clone = uow.registerExistingObject(entityB, descriptor);
        verify(storageMock).persist(entityA.getUri(), entityA, descriptor);
        assertTrue(uow.contains(entityA));
        assertTrue(uow.contains(clone));
        uow.setShouldClearAfterCommit(true);
        uow.commit();

        verify(cacheManagerMock).evictAll();
    }

    @Test
    public void testLoadFieldValue() throws Exception {
        final OWLClassB b = new OWLClassB();
        b.setUri(URI.create("http://bUri"));
        final String stringAtt = "string";
        final OWLClassB clone = (OWLClassB) uow.registerExistingObject(b, descriptor);
        final Field strField = OWLClassB.getStrAttField();
        strField.setAccessible(true);
        doAnswer(invocation -> {
            final Field f = (Field) invocation.getArguments()[1];
            f.set(invocation.getArguments()[0], stringAtt);
            return null;
        }).when(storageMock).loadFieldValue(clone, strField, descriptor);

        uow.loadEntityField(clone, strField);
        assertNotNull(clone.getStringAttribute());
        verify(storageMock).loadFieldValue(clone, strField, descriptor);
    }

    @Test
    public void testLoadFieldValueManagedType() throws Exception {
        final OWLClassD d = new OWLClassD();
        d.setUri(URI.create("http://dUri"));
        final OWLClassD clone = (OWLClassD) uow.registerExistingObject(d, descriptor);
        final Field toLoad = OWLClassD.getOwlClassAField();
        doAnswer(invocation -> {
            final Field f = (Field) invocation.getArguments()[1];
            f.set(invocation.getArguments()[0], entityA);
            return null;
        }).when(storageMock).loadFieldValue(clone, toLoad, descriptor);

        uow.loadEntityField(clone, toLoad);
        verify(storageMock).loadFieldValue(clone, toLoad, descriptor);
        assertNotNull(clone.getOwlClassA());
        // Verify that the loaded value was cloned
        assertNotSame(entityA, clone.getOwlClassA());
        assertTrue(uow.contains(clone.getOwlClassA()));
    }

    @Test
    public void findOfObjectAlreadyManagedAsLazilyLoadedValueReturnSameObject() throws Exception {
        final OWLClassD d = new OWLClassD();
        d.setUri(URI.create("http://dUri"));
        final OWLClassD clone = (OWLClassD) uow.registerExistingObject(d, descriptor);
        assertNull(clone.getOwlClassA());
        final Field toLoad = OWLClassD.getOwlClassAField();
        doAnswer(invocation -> {
            final Field f = (Field) invocation.getArguments()[1];
            f.set(invocation.getArguments()[0], entityA);
            return null;
        }).when(storageMock).loadFieldValue(clone, toLoad, descriptor);
        uow.loadEntityField(clone, toLoad);
        assertNotNull(clone.getOwlClassA());

        final OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(clone.getOwlClassA(), res);
    }

    @Test(expected = OWLPersistenceException.class)
    public void testLoadFieldValueNotRegistered() throws Exception {
        try {
            uow.loadEntityField(entityB, OWLClassB.getStrAttField());
        } finally {
            verify(storageMock, never()).loadFieldValue(any(Object.class),
                    eq(OWLClassB.getStrAttField()), eq(descriptor));
        }
    }

    @Test
    public void testAttributeChanged() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        final Field strField = OWLClassA.getStrAttField();

        uow.attributeChanged(clone, strField);
        verify(storageMock).merge(clone, strField, descriptor);
    }

    @Test(expected = OWLPersistenceException.class)
    public void testAttributeChangedNotRegistered() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final Field strField = OWLClassA.getStrAttField();
        try {
            uow.attributeChanged(entityA, strField);
        } finally {
            verify(storageMock, never()).merge(any(Object.class), eq(strField), eq(descriptor));
        }
        fail("This line should not have been reached.");
    }

    @Test(expected = IllegalStateException.class)
    public void testAttributeChangedOutsideTransaction() throws Exception {
        final Field strField = OWLClassA.getStrAttField();
        try {
            uow.attributeChanged(entityA, strField);
        } finally {
            verify(storageMock, never()).merge(any(Object.class), eq(strField), eq(descriptor));
        }
        fail("This line should not have been reached.");
    }

    @Test
    public void testIsConsistent() {
        when(storageMock.isConsistent(CONTEXT_URI)).thenReturn(Boolean.TRUE);
        final boolean res = uow.isConsistent(CONTEXT_URI);
        assertTrue(res);
        verify(storageMock).isConsistent(CONTEXT_URI);
    }

    @Test
    public void testGetContexts() {
        final List<URI> contexts = new ArrayList<>(1);
        contexts.add(CONTEXT_URI);
        when(storageMock.getContexts()).thenReturn(contexts);
        final List<URI> res = uow.getContexts();
        assertSame(contexts, res);
        assertEquals(contexts, res);
        verify(storageMock).getContexts();
    }

    @Test(expected = CardinalityConstraintViolatedException.class)
    public void throwsCardinalityViolationWhenMaximumCardinalityIsViolatedOnCommit() {
        final List<OWLClassA> lst = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#a" + i));
            lst.add(a);
        }
        entityL.setReferencedList(lst);
        uow.registerNewObject(entityL, descriptor);
        try {
            uow.commit();
        } finally {
            verify(storageMock, never()).commit();
        }
    }

    @Test(expected = CardinalityConstraintViolatedException.class)
    public void throwsCardinalityViolationExceptionWhenMinimumCardinalityIsViolatedOnCommit() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final List<OWLClassA> lst = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#a" + i));
            lst.add(a);
        }
        entityL.setSimpleList(lst);
        final OWLClassL clone = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        clone.getSimpleList().clear();
        uow.attributeChanged(clone, OWLClassL.getSimpleListField());
        try {
            uow.commit();
        } finally {
            verify(storageMock, never()).commit();
        }
    }

    @Test
    public void icValidationPassesOnCommitWhenConstraintsAreViolatedAndThenFixedDuringTransaction() throws Exception {
        when(transactionMock.isActive()).thenReturn(Boolean.TRUE);
        final List<OWLClassA> lst = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            lst.add(new OWLClassA(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#a" + i)));
        }
        entityL.setSimpleList(lst);
        final OWLClassL clone = (OWLClassL) uow.registerExistingObject(entityL, descriptor);
        clone.setSimpleList(Collections.emptyList());
        uow.attributeChanged(clone, OWLClassL.getSimpleListField());
        final List<OWLClassA> updatedList = new ArrayList<>();
        for (int i = 100; i < 103; i++) {
            updatedList.add(new OWLClassA(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#a" + i)));
        }
        clone.setSimpleList(updatedList);
        uow.attributeChanged(clone, OWLClassL.getSimpleListField());
        uow.commit();
        verify(storageMock).commit();
    }

    @Test
    public void clearCleansUpPersistenceContext() throws Exception {
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
        assertTrue(getMap("newObjectsOriginalToClone") == null || getMap("newObjectsOriginalToClone").isEmpty());
        assertTrue(getMap("newObjectsKeyToClone") == null || getMap("newObjectsKeyToClone").isEmpty());
        assertFalse(getBoolean("hasChanges"));
        assertFalse(getBoolean("hasNew"));
        assertFalse(getBoolean("hasDeleted"));
    }

    private Map<?, ?> getMap(String fieldName) throws Exception {
        final Field field = uow.getClass().getDeclaredField(fieldName);
        if (!field.isAccessible()) {
            field.setAccessible(true);
        }
        return (Map<?, ?>) field.get(uow);
    }


    private boolean getBoolean(String fieldName) throws Exception {
        final Field field = uow.getClass().getDeclaredField(fieldName);
        if (!field.isAccessible()) {
            field.setAccessible(true);
        }
        return (boolean) field.get(uow);
    }

    @Test
    public void unwrapReturnsItselfWhenClassMatches() {
        assertSame(uow, uow.unwrap(UnitOfWork.class));
    }

    @Test
    public void releaseRemovesIndirectCollectionsFromManagedEntities() {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor, false)))
                .thenReturn(entityA);
        final OWLClassA result = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(result);
        assertTrue(result.getTypes() instanceof IndirectSet);
        uow.release();
        assertFalse(result.getTypes() instanceof IndirectSet);
    }

    @Test
    public void rollbackDetachesAllManagedEntities() {
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
    public void registerReplacesAlsoInheritedCollectionInstancesWithIndirectVersions() {
        final OWLClassR entityR = new OWLClassR(Generators.createIndividualIdentifier());
        entityR.setTypes(Generators.generateTypes(5));
        when(storageMock.find(new LoadingParameters<>(OWLClassR.class, entityR.getUri(), descriptor)))
                .thenReturn(entityR);
        final OWLClassR clone = uow.readObject(OWLClassR.class, entityR.getUri(), descriptor);
        assertTrue(clone.getTypes() instanceof IndirectSet);
    }

    @Test
    public void commitPutsIntoCacheInstanceMergedAsDetachedDuringTransaction() {
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
    public void clearResetsCloneBuilder() {
        uow.registerExistingObject(entityA, descriptor);
        uow.clear();
        verify(cloneBuilder).reset();
    }

    @Test
    public void registerExistingObjectInvokesPostCloneListeners() {
        final Consumer<Object> plVerifier = mock(Consumer.class);
        final Object result = uow.registerExistingObject(entityA, descriptor, Collections.singletonList(plVerifier));
        verify(plVerifier).accept(result);
    }

    @Test
    public void registerExistingObjectPassesPostCloneListenersToCloneBuilder() {
        final Consumer<Object> plVerifier = mock(Consumer.class);
        uow.registerExistingObject(entityA, descriptor, Collections.singletonList(plVerifier));
        final ArgumentCaptor<CloneConfiguration> captor = ArgumentCaptor.forClass(CloneConfiguration.class);
        verify(cloneBuilder).buildClone(eq(entityA), captor.capture());
        assertTrue(captor.getValue().getPostRegister().contains(plVerifier));
    }

    @Test
    public void refreshThrowsIllegalArgumentForNonManagedInstance() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Cannot call refresh on an instance not managed by this persistence context.");
        uow.refreshObject(Generators.generateOwlClassAInstance());
    }

    @Test
    public void refreshThrowsIllegalArgumentForRemovedInstance() {
        final Object a = uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(a);
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Cannot call refresh on an instance not managed by this persistence context.");
        uow.refreshObject(a);
    }

    @Test
    public void refreshAcquiresNewConnectionToGetAccessToNonTransactionalEntityState() {
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        a.setStringAttribute("updatedString");
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute(entityA.getStringAttribute());
        original.setTypes(new HashSet<>(entityA.getTypes()));
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(original);
        uow.refreshObject(a);
        // First invocation is when UoW is instantiated
        verify(serverSessionStub, times(2)).acquireConnection();
    }

    @Test
    public void refreshLoadsInstanceFromRepositoryAndOverwritesFieldChanges() {
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
    public void refreshOverwritesObjectPropertyChanges() {
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
    public void refreshSetsUpdatesCloneMapppingForRefreshedInstance() {
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
    public void refreshThrowsEntityNotFoundForNonExistentEntity() {
        final OWLClassD d = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        final LoadingParameters<OWLClassD> loadingParams =
                new LoadingParameters<>(OWLClassD.class, d.getUri(), descriptor, true);
        loadingParams.bypassCache();
        when(storageMock.find(loadingParams)).thenReturn(null);
        thrown.expect(EntityNotFoundException.class);
        thrown.expectMessage("Entity " + d + " no longer exists in the repository.");

        uow.refreshObject(d);
    }

    @Test
    public void refreshCancelsObjectChangesInUnitOfWorkChangeSet() throws Exception {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        a.setStringAttribute("updatedString");
        uow.attributeChanged(a, OWLClassA.getStrAttField());
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute(entityA.getStringAttribute());
        original.setTypes(new HashSet<>(entityA.getTypes()));
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(original);
        final UnitOfWorkChangeSet uowChangeSet = uow.getUowChangeSet();
        assertNotNull(uowChangeSet.getExistingObjectChanges(entityA));
        uow.refreshObject(a);
        assertNull(uowChangeSet.getExistingObjectChanges(entityA));
        assertNull(uowChangeSet.getExistingObjectChanges(original));
    }

    @Test
    public void refreshOverwritesChangesSentToRepository() throws Exception {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        a.setStringAttribute("updatedString");
        final OWLClassA original = new OWLClassA(entityA.getUri());
        original.setStringAttribute(entityA.getStringAttribute());
        original.setTypes(new HashSet<>(entityA.getTypes()));
        Mockito.reset(storageMock);
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(original);
        uow.refreshObject(a);
        verify(storageMock).merge(eq(a), eq(OWLClassA.getStrAttField()), any(Descriptor.class));
    }

    @Test
    public void restoreDeletedRegistersObjectAgain() {
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(a);

        uow.restoreRemovedObject(a);
        assertTrue(uow.contains(a));
        assertSame(entityA, uow.getOriginal(a));
    }

    @Test
    public void restoreDeletedReinsertsObjectIntoRepository() {
        final OWLClassA a = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(a);
        uow.restoreRemovedObject(a);
        verify(storageMock).persist(a.getUri(), a, descriptor);
    }

    @Test
    public void commitDetachesPersistedInstance() {
        uow.registerNewObject(entityA, descriptor);
        assertTrue(entityA.getTypes() instanceof IndirectSet);
        assertTrue(uow.contains(entityA));
        uow.commit();
        assertFalse(uow.contains(entityA));
    }

    @Test
    public void commitReplacesIndirectCollectionsWithRegularOnesInDetachedInstances() {
        uow.registerNewObject(entityA, descriptor);
        assertTrue(entityA.getTypes() instanceof IndirectSet);
        uow.commit();
        assertFalse(entityA.getTypes() instanceof IndirectSet);
    }

    @Test
    public void detachReplacesInheritedIndirectCollectionWithRegularOne() {
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
}
