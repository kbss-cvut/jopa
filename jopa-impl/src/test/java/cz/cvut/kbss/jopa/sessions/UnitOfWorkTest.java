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

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityManagerImpl;
import cz.cvut.kbss.jopa.model.EntityManagerImpl.State;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.ontodriver.exception.PrimaryKeyNotSetException;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class UnitOfWorkTest {

    static final URI CONTEXT_URI = URI.create("http://testContext");

    Descriptor descriptor;

    static OWLClassA entityA;
    static OWLClassB entityB;
    static OWLClassD entityD;
    OWLClassL entityL;

    @Mock
    Metamodel metamodelMock;
    @Mock
    CacheManager cacheManagerMock;
    @Mock
    ConnectionWrapper storageMock;
    @Mock
    EntityManagerImpl emMock;
    @Mock
    EntityTransaction transactionMock;

    UnitOfWorkImpl uow;

    @BeforeClass
    public static void setUpBeforeClass() {
        final URI pkOne = URI.create("http://testOne");
        entityA = new OWLClassA();
        entityA.setUri(pkOne);
        entityA.setStringAttribute("attribute");
        entityA.setTypes(new HashSet<>());
        entityA.getTypes().add("http://krizik.felk.cvut.cz/ontologies/jopa#entityQ");
        entityA.getTypes().add("http://krizik.felk.cvut.cz/ontologies/jopa#entityX");
        entityA.getTypes().add("http://krizik.felk.cvut.cz/ontologies/jopa#entityW");
        final URI pkTwo = URI.create("http://testTwo");
        entityB = new OWLClassB();
        entityB.setUri(pkTwo);
        final URI pkThree = URI.create("http://testThree");
        entityD = new OWLClassD();
        entityD.setUri(pkThree);
        entityD.setOwlClassA(entityA);
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.entityL = new OWLClassL();
        this.descriptor = new EntityDescriptor(CONTEXT_URI);
        entityL.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityL"));
        final ServerSessionStub ssStub = new ServerSessionStub(mock(ConnectionWrapper.class));
        ServerSessionStub serverSessionMock = spy(ssStub);
        when(serverSessionMock.getMetamodel()).thenReturn(metamodelMock);
        when(serverSessionMock.getLiveObjectCache()).thenReturn(cacheManagerMock);
        when(emMock.getTransaction()).thenReturn(transactionMock);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        uow = new UnitOfWorkImpl(serverSessionMock);
        uow.setEntityManager(emMock);
        final Field connectionField = UnitOfWorkImpl.class.getDeclaredField("storage");
        connectionField.setAccessible(true);
        connectionField.set(uow, storageMock);
    }

    @Test
    public void testReadObjectFromCache() {
        when(cacheManagerMock.get(OWLClassA.class, entityA.getUri(), CONTEXT_URI)).thenReturn(
                entityA);
        OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertEquals(res.getStringAttribute(), entityA.getStringAttribute());
    }

    @SuppressWarnings("unchecked")
    @Test(expected = NullPointerException.class)
    public void testReadObjectNullPrimaryKey() {
        try {
            uow.readObject(entityA.getClass(), null, descriptor);
        } finally {
            verify(cacheManagerMock, never()).get(any(), any(), any());
        }
        fail("This line should not have been reached.");
    }

    @SuppressWarnings("unchecked")
    @Test(expected = NullPointerException.class)
    public void testReadObjectNullClass() {
        try {
            uow.readObject(null, entityB.getUri(), descriptor);
        } finally {
            verify(cacheManagerMock, never()).get(any(), any(), any());
        }
        fail("This line should not have been reached.");
    }

    @SuppressWarnings("unchecked")
    @Test(expected = NullPointerException.class)
    public void testReadObjectNullContext() {
        try {
            uow.readObject(entityA.getClass(), entityA.getUri(), null);
        } finally {
            verify(cacheManagerMock, never()).get(any(), any(), any());
        }
        fail("This line should not have been reached.");
    }

    @Test
    public void testReadObjectFromOntology() throws Exception {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor)))
                .thenReturn(entityA);
        OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getUri());
        verify(cacheManagerMock).get(OWLClassA.class, entityA.getUri(), CONTEXT_URI);
    }

    @Test
    public void testReadObjectJustPersisted() throws Exception {
        uow.registerNewObject(entityA, descriptor);
        assertTrue(uow.contains(entityA));
        final OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(entityA, res);
    }

    @Test
    public void readAlreadyManagedObjectReturnsTheManagedOne() throws Exception {
        final OWLClassA clone = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertNotNull(clone);
        final OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertSame(clone, res);
        // Had to cast, otherwise ajc refused to compile this
        verify(storageMock, never()).find((LoadingParameters<OWLClassA>) any(LoadingParameters.class));
    }

    @Test
    public void testCalculateNewObjects() throws Exception {
        uow.registerNewObject(entityA, descriptor);
        uow.registerNewObject(entityB, descriptor);
        uow.registerNewObject(entityD, descriptor);
        uow.commit();

        ArgumentCaptor<Object> pks = ArgumentCaptor.forClass(Object.class);
        verify(cacheManagerMock, times(3)).add(pks.capture(), any(Object.class), eq(CONTEXT_URI));
        final Set<URI> uris = pks.getAllValues().stream().map(pk -> URI.create(pk.toString())).collect(
                Collectors.toSet());
        assertTrue(uris.contains(entityA.getUri()));
        assertTrue(uris.contains(entityB.getUri()));
        assertTrue(uris.contains(entityD.getUri()));
    }

    @Test
    public void testCalculateDeletedObjects() throws Exception {
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
        verify(cacheManagerMock).add(eq(newA.getUri()), any(Object.class),
                eq(CONTEXT_URI));
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
    public void testContains() throws Exception {
        OWLClassA res = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertNotNull(res);
        assertTrue(uow.contains(res));
    }

    @Test
    public void testGetState() throws Exception {
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
    public void testGetStateWithDescriptor() throws Exception {
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
    public void testGetOriginal() throws Exception {
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
    public void getManagedOriginalReturnsManagedOriginalInstance() throws Exception {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor))).thenReturn(
                entityA);
        uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);

        final OWLClassA res = uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(entityA, res);
    }

    @Test
    public void getManagedOriginalForDifferentContextReturnsNull() throws Exception {
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor))).thenReturn(
                entityA);
        uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);

        final EntityDescriptor differentContext = new EntityDescriptor(URI.create("http://differentContext"));
        assertNull(uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), differentContext));
    }

    @Test
    public void getManagedOriginalForUnknownIdentifierReturnsNull() throws Exception {
        assertNull(uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor));
    }

    @Test
    public void testIsObjectNew() throws Exception {
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
    public void testRegisterNewObject() throws Exception {
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
    public void testRegisterNewObjectNullPkNotGenerated() throws Exception {
        final OWLClassB b = new OWLClassB();
        try {
            uow.registerNewObject(b, descriptor);
        } finally {
            verify(storageMock, never()).persist(any(Object.class), any(Object.class),
                    eq(descriptor));
        }
        fail("This line should not have been reached.");
    }

    @Test(expected = OWLEntityExistsException.class)
    public void testRegisterNewObjectAlreadyExists() {
        uow.registerNewObject(entityB, descriptor);
        uow.registerNewObject(entityB, descriptor);
        fail("This line should not have been reached.");
    }

    @Test
    public void testReleaseUnitOfWork() throws Exception {
        assertTrue(uow.isActive());
        uow.release();
        assertFalse(uow.isActive());
        verify(storageMock).close();
    }

    @Test
    public void testRemoveObject() throws Exception {
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
        fail("This line should not have been reached.");
    }

    @Test
    public void testUnregisterObject() {
        final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertTrue(uow.contains(managed));
        this.uow.unregisterObject(managed);
        assertFalse(uow.contains(managed));
    }

    @Test
    public void testRevertObject() {
        final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertNotNull(managed);
        final String changedAtt = "changedAtt";
        managed.setStringAttribute(changedAtt);
        this.uow.revertObject(managed);
        assertEquals(entityA.getStringAttribute(), managed.getStringAttribute());
    }

    @Test
    public void revertObjectReference() throws Exception {
        OWLClassD clone = (OWLClassD) uow.registerExistingObject(entityD, descriptor);
        OWLClassA changedRef = new OWLClassA();
        final URI pk = URI.create("http://changedOne");
        changedRef.setStringAttribute("changedAtt");
        changedRef.setUri(pk);
        clone.setOwlClassA(changedRef);
        this.uow.revertObject(clone);
        assertEquals(entityA.getUri(), clone.getOwlClassA().getUri());
        assertEquals(entityA.getStringAttribute(), clone.getOwlClassA().getStringAttribute());
        assertNotNull(clone.getOwlClassA().getTypes());
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
    public void testCommitInactive() throws Exception {
        uow.release();
        uow.commit();
        fail("This line should not have been reached.");
    }

    @Test
    public void testRollback() throws Exception {
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
    public void testRollbackInactive() throws Exception {
        uow.release();
        uow.rollback();
        fail("This line should not have been reached.");
    }

    @Test(expected = OWLPersistenceException.class)
    public void testCommitFailed() throws Exception {
        doThrow(OWLPersistenceException.class).when(storageMock).commit();
        try {
            uow.commit();
        } finally {
            verify(emMock).removeCurrentPersistenceContext();
        }
    }

    @Test
    public void testClearCacheAfterCommit() throws Exception {
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
        assertNull(clone.getStringAttribute());
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
        assertNull(clone.getOwlClassA());
        final Field toLoad = OWLClassD.getOwlClassAField();
        doAnswer(invocation -> {
            final Field f = (Field) invocation.getArguments()[1];
            f.set(invocation.getArguments()[0], entityA);
            return null;
        }).when(storageMock).loadFieldValue(clone, toLoad, descriptor);

        uow.loadEntityField(clone, toLoad);
        assertNotNull(clone.getOwlClassA());
        // Verify that the loaded value was cloned
        assertNotSame(entityA, clone.getOwlClassA());
        assertTrue(uow.contains(clone.getOwlClassA()));
        verify(storageMock).loadFieldValue(clone, toLoad, descriptor);
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
    public void testMergeDetachedExisting() throws Exception {
        mergeDetachedTest();
    }

    @SuppressWarnings("unchecked")
    private void mergeDetachedTest() throws Exception {
        when(storageMock.contains(entityA.getUri(), entityA.getClass(), descriptor))
                .thenReturn(Boolean.TRUE);
        final OWLClassA orig = new OWLClassA();
        orig.setUri(entityA.getUri());
        orig.setStringAttribute("oldStringAttribute");
        orig.setTypes(new HashSet<>(entityA.getTypes()));
        final Iterator<String> it = orig.getTypes().iterator();
        it.next();
        it.remove();
        when(storageMock.find(new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor, true)))
                .thenReturn(orig);

        final OWLClassA res = uow.mergeDetached(entityA, descriptor);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getUri());
        final ArgumentCaptor<Field> ac = ArgumentCaptor.forClass(Field.class);
        verify(storageMock, atLeastOnce()).merge(any(Object.class), ac.capture(), eq(descriptor));
        final List<Field> mergedFields = ac.getAllValues();
        assertTrue(mergedFields.contains(OWLClassA.getStrAttField()));
        assertTrue(mergedFields.contains(OWLClassA.getTypesField()));
    }

    @Test
    public void testMergeDetachedEvictFromCache() throws Exception {
        when(cacheManagerMock.contains(OWLClassA.class, entityA.getUri(), CONTEXT_URI))
                .thenReturn(Boolean.TRUE);
        mergeDetachedTest();
        verify(cacheManagerMock).evict(OWLClassA.class, entityA.getUri(), CONTEXT_URI);
    }

    @Test
    public void testMergeDetachedNew() throws Exception {
        when(storageMock.contains(entityA.getUri(), entityA.getClass(), descriptor))
                .thenReturn(Boolean.FALSE);
        final OWLClassA res = uow.mergeDetached(entityA, descriptor);
        assertNotNull(res);
        assertSame(entityA, res);
        verify(storageMock).persist(entityA.getUri(), entityA, descriptor);
    }

    @Test
    public void testIsConsistent() throws Exception {
        when(storageMock.isConsistent(CONTEXT_URI)).thenReturn(Boolean.TRUE);
        final boolean res = uow.isConsistent(CONTEXT_URI);
        assertTrue(res);
        verify(storageMock).isConsistent(CONTEXT_URI);
    }

    @Test
    public void testGetContexts() throws Exception {
        final List<URI> contexts = new ArrayList<>(1);
        contexts.add(CONTEXT_URI);
        when(storageMock.getContexts()).thenReturn(contexts);
        final List<URI> res = uow.getContexts();
        assertSame(contexts, res);
        assertEquals(contexts, res);
        verify(storageMock).getContexts();
    }

    @Test(expected = CardinalityConstraintViolatedException.class)
    public void throwsCardinalityViolationWhenMaximumCardinalityIsViolatedOnCommit() throws Exception {
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
        assertTrue(getMap("cloneMapping") == null || getMap("cloneMapping").isEmpty());
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
}
