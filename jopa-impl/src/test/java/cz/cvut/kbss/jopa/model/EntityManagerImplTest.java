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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassE;
import cz.cvut.kbss.jopa.environment.OWLClassH;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.OWLClassK;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityLifecycleListenerManager;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingSetProxy;
import cz.cvut.kbss.jopa.sessions.ChangeTrackingUnitOfWork;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.sessions.ServerSessionStub;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.NamespaceResolver;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class EntityManagerImplTest {

    private static final String SELECT_QUERY = "SELECT * WHERE { ?x a <" + Vocabulary.c_OwlClassA + "> . }";

    private static final String NON_ENTITY_CLASS_EXCEPTION_MESSAGE =
            "Class " + UnknownEntity.class.getName() + " is not a known entity in this persistence unit.";

    @Mock
    private EntityManagerFactoryImpl emfMock;

    @Mock
    private ConnectionWrapper connectorMock;

    private UnitOfWork uow;

    @Mock
    private MetamodelImpl metamodelMock;

    private EntityDescriptorFactory descriptorFactory;

    private MetamodelMocks mocks;

    private EntityManagerImpl em;

    @BeforeEach
    void setUp() throws Exception {
        final Configuration config = new Configuration();
        final ServerSession serverSessionMock = spy(new ServerSessionStub(metamodelMock, connectorMock));
        this.uow = spy(new ChangeTrackingUnitOfWork(serverSessionMock, config));
        doReturn(uow).when(serverSessionMock).acquireUnitOfWork(any());
        when(emfMock.getMetamodel()).thenReturn(metamodelMock);
        when(emfMock.getServerSession()).thenReturn(serverSessionMock);
        final NamespaceResolver nsResolver = new NamespaceResolver();
        this.descriptorFactory = new DefaultEntityDescriptorFactory(metamodelMock, nsResolver);
        when(metamodelMock.getNamespaceResolver()).thenReturn(nsResolver);
        this.mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        this.em = new EntityManagerImpl(emfMock, config, descriptorFactory);
    }

    @Test
    void testCascadeMergeOnNullCollection() {
        final OWLClassJ j = new OWLClassJ();
        j.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityJ"));
        mocks.forOwlClassJ().setAttribute().getJavaField().setAccessible(true);
        assertNull(j.getOwlClassA());

        em.merge(j);
        final ArgumentCaptor<OWLClassJ> argumentCaptor = ArgumentCaptor.forClass(OWLClassJ.class);
        verify(uow).mergeDetached(argumentCaptor.capture(), any(Descriptor.class));
        assertSame(j, argumentCaptor.getValue());
        // Check that there is no exception thrown (there was a NPX bug in merging null collections) and that
        // the merged object is correctly passed to merge in UoW
    }

    @Test
    void cascadeMergeDoesNothingForLazyLoadingProxy() {
        final OWLClassJ j = new OWLClassJ(Generators.createIndividualIdentifier());
        j.setOwlClassA(new LazyLoadingSetProxy<>(j, mocks.forOwlClassJ().setAttribute(), uow));

        em.merge(j);
        verify(uow).mergeDetached(eq(j), any(EntityDescriptor.class));
    }

    @Test
    void mergeDetachedWithSingletonSet() {
        final OWLClassJ j = new OWLClassJ();
        j.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityF"));
        final OWLClassA a = new OWLClassA(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityA"));
        j.setOwlClassA(Collections.singleton(a));

        final OWLClassJ merged = em.merge(j);
        assertSame(j, merged);
        verify(uow).mergeDetached(eq(j), any());
        verify(uow).mergeDetached(eq(a), any());
    }

    @Test
    void mergeDetachedWithSingletonList() {
        final OWLClassC c = new OWLClassC(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityF"));
        final OWLClassA a = new OWLClassA(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityA"));
        c.setSimpleList(Collections.singletonList(a));
        // Just for this test
        when(mocks.forOwlClassC().simpleListAtt().getCascadeTypes()).thenReturn(new CascadeType[]{CascadeType.MERGE});

        final OWLClassC merged = em.merge(c);
        assertSame(c, merged);
        verify(uow).mergeDetached(eq(c), any());
        verify(uow).mergeDetached(eq(a), any());
    }

    @Test
    void unwrapReturnsItselfWhenClassMatches() {
        assertSame(em, em.unwrap(EntityManagerImpl.class));
    }

    @Test
    void containsThrowsIllegalArgumentForNonEntity() {
        final UnknownEntity obj = new UnknownEntity();
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> em.contains(obj));
        assertEquals(NON_ENTITY_CLASS_EXCEPTION_MESSAGE, ex.getMessage());
    }


    @Test
    void findThrowsIllegalArgumentForNonEntity() {
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> em.find(UnknownEntity.class, "identifier", new EntityDescriptor()));
        assertEquals(NON_ENTITY_CLASS_EXCEPTION_MESSAGE, ex.getMessage());
    }

    @Test
    void persistThrowsIllegalArgumentForNonEntity() {
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> em.persist(new UnknownEntity(), new EntityDescriptor()));
        assertEquals(NON_ENTITY_CLASS_EXCEPTION_MESSAGE, ex.getMessage());
    }

    @Test
    void mergeThrowsIllegalArgumentForNonEntity() {
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> em.merge(new UnknownEntity(), new EntityDescriptor()));
        assertEquals(NON_ENTITY_CLASS_EXCEPTION_MESSAGE, ex.getMessage());
    }

    @Test
    void removeThrowsIllegalArgumentForNonEntity() {
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> em.remove(new UnknownEntity()));
        assertEquals(NON_ENTITY_CLASS_EXCEPTION_MESSAGE, ex.getMessage());
    }

    @Test
    void refreshThrowsIllegalArgumentForNonEntity() {
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> em.refresh(new UnknownEntity()));
        assertEquals(NON_ENTITY_CLASS_EXCEPTION_MESSAGE, ex.getMessage());
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#UnknownEntity")
    private static class UnknownEntity {
        @Id
        private URI id;

        public URI getId() {
            return id;
        }

        public void setId(URI id) {
            this.id = id;
        }
    }

    @Test
    void closeNotifiesEntityManagerFactoryOfClosing() {
        assertTrue(em.isOpen());
        em.close();
        assertFalse(em.isOpen());
        verify(emfMock).entityManagerClosed(em);
    }

    @Test
    void exceptionInPersistMarksTransactionForRollbackOnly() {
        final EntityTransaction tx = em.getTransaction();
        doThrow(OWLPersistenceException.class).when(uow).registerNewObject(any(), any());
        try {
            tx.begin();
            em.persist(Generators.generateOwlClassAInstance());
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        assertTrue(tx.isRollbackOnly());
    }

    @Test
    void exceptionInCascadePersistMarksTransactionForRollbackOnly() {
        final OWLClassH h = new OWLClassH(Generators.createIndividualIdentifier());
        h.setOwlClassA(Generators.generateOwlClassAInstance());
        final EntityTransaction tx = em.getTransaction();
        doThrow(OWLPersistenceException.class).when(uow).registerNewObject(eq(h.getOwlClassA()), any());
        try {
            tx.begin();
            em.persist(h);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        assertTrue(tx.isRollbackOnly());
    }

    @Test
    void exceptionInFindMarksTransactionForRollbackOnly() {
        final EntityTransaction tx = em.getTransaction();
        doThrow(OWLPersistenceException.class).when(uow).readObject(any(), any(), any());
        try {
            tx.begin();
            em.find(OWLClassA.class, Generators.createIndividualIdentifier());
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        assertTrue(tx.isRollbackOnly());
    }

    @Test
    void exceptionInMergeMarksTransactionForRollbackOnly() {
        final EntityTransaction tx = em.getTransaction();
        doThrow(OWLPersistenceException.class).when(uow).mergeDetached(any(), any());
        try {
            tx.begin();
            em.merge(Generators.generateOwlClassAInstance());
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        assertTrue(tx.isRollbackOnly());
    }

    @Test
    void exceptionInMergeCascadingMarksTransactionForRollbackOnly() {
        final OWLClassH h = new OWLClassH(Generators.createIndividualIdentifier());
        h.setOwlClassA(Generators.generateOwlClassAInstance());
        final EntityTransaction tx = em.getTransaction();
        doReturn(h).when(uow).mergeDetached(eq(h), any());
        doThrow(OWLPersistenceException.class).when(uow).mergeDetached(eq(h.getOwlClassA()), any());
        try {
            tx.begin();
            em.merge(h);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        assertTrue(tx.isRollbackOnly());
    }

    @Test
    void exceptionInRemoveMarksTransactionForRollbackOnly() {
        doThrow(OWLPersistenceException.class).when(uow).removeObject(any());
        final EntityTransaction tx = em.getTransaction();
        try {
            tx.begin();
            assertFalse(tx.isRollbackOnly());
            em.remove(Generators.generateOwlClassAInstance());
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        assertTrue(tx.isRollbackOnly());
    }

    @Test
    void exceptionInFlushMarksTransactionForRollbackOnly() {
        doThrow(OWLPersistenceException.class).when(uow).writeUncommittedChanges();
        final EntityTransaction tx = em.getTransaction();
        try {
            tx.begin();
            em.flush();
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        assertTrue(tx.isRollbackOnly());
    }

    @Test
    void exceptionInRefreshMarksTransactionForRollbackOnly() {
        doThrow(OWLPersistenceException.class).when(uow).refreshObject(any());
        final EntityTransaction tx = em.getTransaction();
        try {
            tx.begin();
            em.refresh(Generators.generateOwlClassAInstance());
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        assertTrue(tx.isRollbackOnly());
    }

    @Test
    void exceptionInDetachMarksTransactionForRollbackOnly() {
        final OWLClassA a = Generators.generateOwlClassAInstance();
        doReturn(EntityState.MANAGED).when(uow).getState(a);
        doThrow(OWLPersistenceException.class).when(uow).unregisterObject(a);
        final EntityTransaction tx = em.getTransaction();
        try {
            tx.begin();
            em.detach(a);
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        assertTrue(tx.isRollbackOnly());
    }

    @Test
    void exceptionInContainsMarksTransactionForRollbackOnly() {
        doThrow(OWLPersistenceException.class).when(uow).contains(any());
        final EntityTransaction tx = em.getTransaction();
        try {
            tx.begin();
            em.contains(Generators.generateOwlClassAInstance());
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        assertTrue(tx.isRollbackOnly());
    }

    @Test
    void exceptionInClearMarksTransactionForRollbackOnly() {
        doThrow(OWLPersistenceException.class).when(uow).clear();
        final EntityTransaction tx = em.getTransaction();
        try {
            tx.begin();
            em.clear();
        } catch (RuntimeException e) {
            // Swallow the exception
        }
        assertTrue(tx.isRollbackOnly());
    }

    /**
     * Bug #4
     */
    @Test
    void mergeIsAbleToBreakCascadingCycle() throws Exception {
        Mockito.reset(metamodelMock);
        final CascadeCycleOne cOne = new CascadeCycleOne(Generators.createIndividualIdentifier());
        final CascadeCycleTwo cTwo = new CascadeCycleTwo(Generators.createIndividualIdentifier());
        cOne.two = cTwo;
        cTwo.one = cOne;
        metamodelForCascadingTest();
        final CascadeCycleOne cloneOne = new CascadeCycleOne(cOne.uri);
        final CascadeCycleTwo cloneTwo = new CascadeCycleTwo(cTwo.uri);
        cloneOne.two = cloneTwo;
        cloneTwo.one = cloneOne;
        doReturn(EntityState.NOT_MANAGED).when(uow).getState(cOne);
        doReturn(EntityState.NOT_MANAGED).when(uow).getState(cTwo);
        doReturn(EntityState.MANAGED).when(uow).getState(cloneOne);
        doReturn(EntityState.MANAGED).when(uow).getState(cloneTwo);
        doReturn(cloneOne).when(uow).mergeDetached(eq(cOne), any());
        doReturn(cloneTwo).when(uow).mergeDetached(eq(cTwo), any());
        doReturn(cloneOne).when(uow).getCloneForOriginal(cOne);
        doReturn(cloneTwo).when(uow).getCloneForOriginal(cTwo);
        em.merge(cOne);
        verify(uow).mergeDetached(eq(cOne), any());
        verify(uow).mergeDetached(eq(cTwo), any());
    }

    private void metamodelForCascadingTest() throws Exception {
        final IdentifiableEntityType<CascadeCycleOne> etOne = mock(IdentifiableEntityType.class);
        final Identifier idOne = mock(Identifier.class);
        when(idOne.getJavaField()).thenReturn(CascadeCycleOne.class.getDeclaredField("uri"));
        when(etOne.getIdentifier()).thenReturn(idOne);
        final Attribute<CascadeCycleOne, CascadeCycleTwo> attOne = mock(Attribute.class);
        when(attOne.getCascadeTypes())
                .thenReturn(new CascadeType[]{CascadeType.PERSIST, CascadeType.MERGE, CascadeType.REMOVE});
        when(attOne.getJavaField()).thenReturn(CascadeCycleOne.class.getDeclaredField("two"));
        when(attOne.getName()).thenReturn("two");
        when(attOne.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(attOne.getJavaType()).thenReturn(CascadeCycleTwo.class);
        when(attOne.getJavaMember()).thenReturn(CascadeCycleOne.class.getDeclaredField("two"));
        when(etOne.getAttributes()).thenReturn(Collections.singleton(attOne));
        when(etOne.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
        final IdentifiableEntityType<CascadeCycleTwo> etTwo = mock(IdentifiableEntityType.class);
        final Identifier idTwo = mock(Identifier.class);
        when(idTwo.getJavaField()).thenReturn(CascadeCycleTwo.class.getDeclaredField("uri"));
        when(etTwo.getIdentifier()).thenReturn(idTwo);
        final Attribute<CascadeCycleTwo, CascadeCycleOne> attTwo = mock(Attribute.class);
        when(attTwo.getCascadeTypes())
                .thenReturn(new CascadeType[]{CascadeType.PERSIST, CascadeType.MERGE, CascadeType.REMOVE});
        when(attTwo.getJavaField()).thenReturn(CascadeCycleTwo.class.getDeclaredField("one"));
        when(attTwo.getName()).thenReturn("one");
        when(attTwo.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(attTwo.getJavaType()).thenReturn(CascadeCycleOne.class);
        when(attTwo.getJavaMember()).thenReturn(CascadeCycleTwo.class.getDeclaredField("one"));
        when(etTwo.getAttributes()).thenReturn(Collections.singleton(attTwo));
        when(etTwo.getLifecycleListenerManager()).thenReturn(EntityLifecycleListenerManager.empty());
        when(metamodelMock.entity(CascadeCycleOne.class)).thenReturn(etOne);
        when(metamodelMock.entity(CascadeCycleTwo.class)).thenReturn(etTwo);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "CascadeCycleOne")
    private static class CascadeCycleOne {
        @Id
        private URI uri;
        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "hasTwo", cascade = {CascadeType.MERGE,
                CascadeType.PERSIST,
                CascadeType.REMOVE})
        private CascadeCycleTwo two;

        private CascadeCycleOne(URI uri) {
            this.uri = uri;
        }
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "CascadeCycleTwo")
    private static class CascadeCycleTwo {
        @Id
        private URI uri;
        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "hasOne", cascade = {CascadeType.PERSIST,
                CascadeType.MERGE,
                CascadeType.REMOVE})
        private CascadeCycleOne one;

        private CascadeCycleTwo(URI uri) {
            this.uri = uri;
        }
    }

    @Test
    void mergeOfNewInstancesIsAbleToBreakCascadingCycle() throws Exception {
        Mockito.reset(metamodelMock);
        final CascadeCycleOne cOne = new CascadeCycleOne(Generators.createIndividualIdentifier());
        final CascadeCycleTwo cTwo = new CascadeCycleTwo(Generators.createIndividualIdentifier());
        cOne.two = cTwo;
        cTwo.one = cOne;
        metamodelForCascadingTest();
        doReturn(EntityState.NOT_MANAGED).doReturn(EntityState.MANAGED_NEW).when(uow)
                                         .getState(cOne);
        doReturn(EntityState.NOT_MANAGED).doReturn(EntityState.MANAGED_NEW).when(uow)
                                         .getState(cTwo);
        doReturn(cOne).when(uow).mergeDetached(eq(cOne), any());
        doReturn(cTwo).when(uow).mergeDetached(eq(cTwo), any());
        doReturn(cOne).when(uow).getCloneForOriginal(cOne);
        doReturn(cTwo).when(uow).getCloneForOriginal(cTwo);
        em.merge(cOne);
        verify(uow).mergeDetached(eq(cOne), any());
        verify(uow).mergeDetached(eq(cTwo), any());
    }

    @Test
    void removeIsAbleToBreakCascadingCycle() throws Exception {
        Mockito.reset(metamodelMock);
        final CascadeCycleOne cOne = new CascadeCycleOne(Generators.createIndividualIdentifier());
        final CascadeCycleTwo cTwo = new CascadeCycleTwo(Generators.createIndividualIdentifier());
        cOne.two = cTwo;
        cTwo.one = cOne;
        metamodelForCascadingTest();
        final CascadeCycleOne cloneOne = new CascadeCycleOne(cOne.uri);
        final CascadeCycleTwo cloneTwo = new CascadeCycleTwo(cTwo.uri);
        cloneOne.two = cloneTwo;
        cloneTwo.one = cloneOne;
        doReturn(EntityState.MANAGED).doReturn(EntityState.REMOVED).when(uow).getState(cloneOne);
        doReturn(EntityState.MANAGED).doReturn(EntityState.REMOVED).when(uow).getState(cloneTwo);
        doNothing().when(uow).removeObject(any());
        em.remove(cloneOne);
        verify(uow).removeObject(cloneOne);
        verify(uow).removeObject(cloneTwo);
    }

    @Test
    void isLoadedReturnsTrueForEagerlyLoadedAttributeOfManagedInstance() throws Exception {
        final OWLClassA a = Generators.generateOwlClassAInstance();
        doAnswer((invocationOnMock) -> a).when(uow)
                                         .readObject(eq(OWLClassA.class), eq(a.getUri()), any(Descriptor.class));
        doReturn(LoadState.LOADED).when(uow).isLoaded(a, OWLClassA.getStrAttField().getName());
        final OWLClassA found = em.find(OWLClassA.class, a.getUri());
        assertTrue(em.isLoaded(found, OWLClassA.getStrAttField().getName()));
    }

    @Test
    void isLoadedReturnsFalseForNonManagedInstance() throws Exception {
        final OWLClassA a = Generators.generateOwlClassAInstance();
        doReturn(LoadState.UNKNOWN).when(uow).isLoaded(eq(a), anyString());
        assertFalse(em.isLoaded(a, OWLClassA.getStrAttField().getName()));
    }

    @Test
    void isLoadedReturnsTrueForNonNullLazilyLoadedAttribute() throws Exception {
        final OWLClassK inst = new OWLClassK();
        inst.setUri(Generators.createIndividualIdentifier());
        inst.setOwlClassE(new OWLClassE());
        doAnswer((invocationOnMock) -> inst).when(uow)
                                            .readObject(eq(OWLClassK.class), eq(inst.getUri()), any(Descriptor.class));
        doReturn(LoadState.LOADED).when(uow).isLoaded(inst, OWLClassK.getOwlClassEField().getName());
        final OWLClassK found = em.find(OWLClassK.class, inst.getUri());
        assertTrue(em.isLoaded(found, OWLClassK.getOwlClassEField().getName()));
    }

    @Test
    void mergeOfAlreadyPersistedReturnsSameInstance() {
        final OWLClassA a = Generators.generateOwlClassAInstance();
        em.persist(a);
        final OWLClassA result = em.merge(a);
        assertSame(a, result);
    }

    @Test
    void mergeDifferentInstanceWithSameIdAsManagedInstanceMergesChanges() {
        final OWLClassA a = Generators.generateOwlClassAInstance();
        em.persist(a);
        when(connectorMock.contains(eq(a.getUri()), eq(OWLClassA.class), any())).thenReturn(true);
        when(connectorMock.find(any())).thenReturn(a);
        final OWLClassA copyA = new OWLClassA(a.getUri());
        final String str = "differentString";
        copyA.setStringAttribute(str);
        copyA.setTypes(new HashSet<>(a.getTypes()));
        final OWLClassA result = em.merge(copyA);
        assertEquals(str, result.getStringAttribute());
        assertEquals(a.getTypes(), result.getTypes());
        assertTrue(em.contains(result));
        final ArgumentCaptor<OWLClassA> captor = ArgumentCaptor.forClass(OWLClassA.class);
        verify(connectorMock).merge(captor.capture(), eq(mocks.forOwlClassA().stringAttribute()), any());
        assertEquals(a.getUri(), captor.getValue().getUri());
    }

    @Test
    void getTransactionOnClosedEntityManagerIsValid() {
        em.close();
        assertNotNull(em.getTransaction());
    }

    @Test
    void persistOnClosedManagerThrowsIllegalStateException() {
        em.close();
        assertThrows(IllegalStateException.class, () -> em.persist(Generators.generateOwlClassAInstance()));
    }

    @Test
    void createQueryOnClosedManagerThrowsIllegalStateException() {
        em.close();
        assertThrows(IllegalStateException.class, () -> em.createNativeQuery(SELECT_QUERY));
    }

    @Test
    void createNativeQuerySetsEnsureOpenProcedureOnQueryInstance() throws Exception {
        final QueryImpl q = em.createNativeQuery("SELECT * WHERE { ?x a rdf:Resource . }");
        verifyEnsureOpenProcedureSet(q);
    }

    private void verifyEnsureOpenProcedureSet(AbstractQuery q) throws NoSuchFieldException, IllegalAccessException {
        final Field ensureOpenProcedureField = AbstractQuery.class.getDeclaredField("ensureOpenProcedure");
        ensureOpenProcedureField.setAccessible(true);
        assertNotNull(ensureOpenProcedureField.get(q));
    }

    @Test
    void createTypedNativeQuerySetsEnsureOpenProcedureOnQueryInstance() throws Exception {
        final TypedQueryImpl<OWLClassA> q =
                em.createNativeQuery("SELECT * WHERE { ?x a rdf:Resource . }", OWLClassA.class);
        verifyEnsureOpenProcedureSet(q);
    }

    @Test
    void createQuerySetsEnsureOpenProcedureOnQueryInstance() throws Exception {
        final QueryImpl q = em.createQuery("SELECT a FROM OWLClassA a");
        verifyEnsureOpenProcedureSet(q);
    }

    @Test
    void createTypedQuerySetsEnsureOpenProcedureOnQueryInstance() throws Exception {
        final TypedQueryImpl<OWLClassA> q = em.createQuery("SELECT a FROM OWLClassA a", OWLClassA.class);
        verifyEnsureOpenProcedureSet(q);
    }

    @Test
    void getReferenceRetrievesInstanceReferenceFromUnitOfWork() {
        final OWLClassA instance = new OWLClassA(Generators.createIndividualIdentifier());
        doReturn(instance).when(uow).getReference(eq(OWLClassA.class), eq(instance.getUri()), any());
        final OWLClassA result = em.getReference(OWLClassA.class, instance.getUri());
        assertSame(instance, result);
        verify(uow).getReference(OWLClassA.class, instance.getUri(), new EntityDescriptor());
    }

    @Test
    void getReferenceWithDescriptorRetrievesInstanceReferenceFromUnitOfWork() {
        final OWLClassA instance = new OWLClassA(Generators.createIndividualIdentifier());
        final Descriptor descriptor = new EntityDescriptor(Generators.createIndividualIdentifier());
        doReturn(instance).when(uow).getReference(OWLClassA.class, instance.getUri(), descriptor);
        final OWLClassA result = em.getReference(OWLClassA.class, instance.getUri(), descriptor);
        assertSame(instance, result);
        verify(uow).getReference(OWLClassA.class, instance.getUri(), descriptor);
    }

    @Test
    void getReferenceThrowsIllegalArgumentExceptionWhenTargetClassIsNotEntity() {
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> em.getReference(UnknownEntity.class, Generators.createIndividualIdentifier()));
        assertEquals(NON_ENTITY_CLASS_EXCEPTION_MESSAGE, ex.getMessage());
        verify(uow, never()).getReference(any(), any(), any());
    }

    @Test
    void getReferenceThrowsIllegalStateWhenInvokedOnClosedEntityManager() {
        em.close();
        assertThrows(IllegalStateException.class,
                () -> em.getReference(OWLClassA.class, Generators.createIndividualIdentifier()));
        verify(uow, never()).getReference(any(), any(), any());
    }

    @Test
    void entityManagerIsAutoCloseable() {
        try (final EntityManager em = new EntityManagerImpl(emfMock, new Configuration(Collections.emptyMap()), descriptorFactory)) {
            assertTrue(em.isOpen());
        }
        verify(emfMock).entityManagerClosed(any(AbstractEntityManager.class));
    }

    @Test
    void cascadeDetachDoesNothingWithLazyLoadingProxy() {
        final OWLClassJ original = new OWLClassJ(Generators.createIndividualIdentifier());
        original.setOwlClassA(null);
        uow.getLoadStateRegistry()
           .put(original, LoadStateDescriptorFactory.createNotLoaded(original, mocks.forOwlClassJ().entityType()));
        when(connectorMock.find(any(LoadingParameters.class))).thenReturn(original);
        final OWLClassJ toDetach = em.find(OWLClassJ.class, original.getUri());
        em.detach(toDetach);
        assertNotNull(toDetach.getOwlClassA());
        assertTrue(toDetach.getOwlClassA().isEmpty());
    }

    @Test
    void cascadeRefreshLoadsTriggersLazyLoading() {
        final OWLClassJ lazyOriginal = new OWLClassJ(Generators.createIndividualIdentifier());
        lazyOriginal.setOwlClassA(null);
        uow.getLoadStateRegistry()
           .put(lazyOriginal, LoadStateDescriptorFactory.createNotLoaded(lazyOriginal, mocks.forOwlClassJ()
                                                                                            .entityType()));
        when(connectorMock.find(new LoadingParameters<>(OWLClassJ.class, lazyOriginal.getUri(), descriptorFactory.createDescriptor(OWLClassJ.class)))).thenReturn(lazyOriginal);
        final OWLClassJ loadedOriginal = new OWLClassJ(lazyOriginal.getUri());
        final OWLClassA a = Generators.generateOwlClassAInstance();
        loadedOriginal.setOwlClassA(Collections.singleton(a));
        uow.getLoadStateRegistry()
           .put(loadedOriginal, LoadStateDescriptorFactory.createAllLoaded(lazyOriginal, mocks.forOwlClassJ()
                                                                                              .entityType()));
        final LoadingParameters<OWLClassJ> refreshLoadParams = new LoadingParameters<>(OWLClassJ.class, lazyOriginal.getUri(), descriptorFactory.createDescriptor(OWLClassJ.class), true);
        refreshLoadParams.bypassCache();
        when(connectorMock.find(refreshLoadParams)).thenReturn(loadedOriginal);
        final LoadingParameters<OWLClassA> refreshALoadParams = new LoadingParameters<>(OWLClassA.class, a.getUri(), descriptorFactory.createDescriptor(OWLClassJ.class)
                                                                                                                                      .getAttributeDescriptor(mocks.forOwlClassJ()
                                                                                                                                                                   .setAttribute()), true);
        refreshALoadParams.bypassCache();
        when(connectorMock.find(refreshALoadParams)).thenReturn(a);

        final OWLClassJ toRefresh = em.find(OWLClassJ.class, lazyOriginal.getUri());
        em.refresh(toRefresh);
        assertNotNull(toRefresh.getOwlClassA());
        assertEquals(1, toRefresh.getOwlClassA().size());
    }

    @Test
    void cascadeRemoveTriggersLazyLoadingAndCascadesRemoval() {
        final OWLClassJ lazyOriginal = new OWLClassJ(Generators.createIndividualIdentifier());
        lazyOriginal.setOwlClassA(null);
        uow.getLoadStateRegistry()
           .put(lazyOriginal, LoadStateDescriptorFactory.createNotLoaded(lazyOriginal, mocks.forOwlClassJ()
                                                                                            .entityType()));
        when(connectorMock.find(any(LoadingParameters.class))).thenReturn(lazyOriginal);
        final OWLClassJ toRemove = em.find(OWLClassJ.class, lazyOriginal.getUri());
        final OWLClassA a = Generators.generateOwlClassAInstance();
        uow.getLoadStateRegistry()
           .put(a, LoadStateDescriptorFactory.createAllLoaded(a, mocks.forOwlClassA().entityType()));
        doAnswer(inv -> {
            final OWLClassJ target = inv.getArgument(0);
            target.setOwlClassA(new HashSet<>(Set.of(a)));
            return null;
        }).when(connectorMock)
          .loadFieldValue(eq(toRemove), eq(mocks.forOwlClassJ().setAttribute()), any(Descriptor.class));

        em.remove(toRemove);
        verify(connectorMock).remove(eq(toRemove.getUri()), eq(OWLClassJ.class), any(Descriptor.class));
        verify(connectorMock).remove(eq(a.getUri()), eq(OWLClassA.class), any(Descriptor.class));
    }
}
