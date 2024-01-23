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
package cz.cvut.kbss.jopa.feature;

import cz.cvut.kbss.jopa.sessions.CloneConfiguration;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.listener.AnotherListener;
import cz.cvut.kbss.jopa.environment.listener.ConcreteListener;
import cz.cvut.kbss.jopa.environment.listener.ParentListener;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.model.EntityManagerImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.*;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import cz.cvut.kbss.jopa.utils.ReflectionUtils;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.*;

/**
 * Verifies entity lifecycle listener behavior w.r.t. the JPA 2.1 spec.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class EntityLifecycleListenersTest {

    private Descriptor descriptor;

    @Mock
    private EntityTransaction transactionMock;

    @Mock
    private MetamodelImpl metamodelMock;

    @Mock
    private CloneBuilder cloneBuilderMock;

    @Mock
    private ConnectionWrapper storageMock;

    @Mock
    private EntityManagerImpl emMock;

    private MetamodelMocks mocks;

    private UnitOfWorkImpl uow;

    private ParentListener parentListenerMock;
    private ConcreteListener concreteListenerMock;
    private AnotherListener anotherListenerMock;

    @BeforeEach
    public void setUp() throws Exception {
        this.descriptor = new EntityDescriptor();
        final ServerSessionStub serverSessionStub = spy(new ServerSessionStub(storageMock));
        when(serverSessionStub.getMetamodel()).thenReturn(metamodelMock);
        when(serverSessionStub.getLiveObjectCache()).thenReturn(mock(CacheManager.class));
        when(emMock.getTransaction()).thenReturn(transactionMock);
        when(transactionMock.isActive()).thenReturn(true);
        final Configuration config = new Configuration();
        when(emMock.getConfiguration()).thenReturn(config);
        this.mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        this.parentListenerMock = mocks.forOwlClassS().parentListener();
        this.concreteListenerMock = mocks.forOwlClassR().concreteListener();
        this.anotherListenerMock = mocks.forOwlClassR().anotherListener();
        uow = new UnitOfWorkImpl(serverSessionStub, config);
        uow.setEntityManager(emMock);
        TestEnvironmentUtils.setMock(uow, UnitOfWorkImpl.class.getDeclaredField("cloneBuilder"), cloneBuilderMock);
    }

    @Test
    public void prePersistLifecycleListenerIsCalledBeforeInstanceIsInsertedIntoPersistenceContext() throws Exception {
        final Map<Object, Object> mockMap = spy(new HashMap<>());
        TestEnvironmentUtils.setMock(uow, UnitOfWorkImpl.class.getDeclaredField("newObjectsKeyToClone"), mockMap);
        final URI rId = Generators.createIndividualIdentifier();
        final OWLClassR rInstance = spy(new OWLClassR());
        when(storageMock.generateIdentifier(metamodelMock.entity(OWLClassR.class))).thenReturn(rId);
        uow.registerNewObject(rInstance, descriptor);
        final InOrder inOrder = inOrder(rInstance, parentListenerMock, concreteListenerMock, anotherListenerMock,
                storageMock, mockMap);
        inOrder.verify(parentListenerMock).prePersist(rInstance);
        inOrder.verify(concreteListenerMock).prePersist(rInstance);
        inOrder.verify(anotherListenerMock).prePersist(rInstance);
        inOrder.verify(rInstance).prePersist();
        inOrder.verify(mockMap).put(rId, rInstance);
    }

    @Test
    public void preRemoveEntityLifecycleListenerIsCalledBeforeInstanceIsRemovedFromPersistenceContext() {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(createInstance(OWLClassR.class));
        rInstance.setUri(rOriginal.getUri());
        when(cloneBuilderMock.buildClone(eq(rOriginal), any())).thenReturn(rInstance);
        uow.registerExistingObject(rOriginal, descriptor);
        uow.removeObject(rInstance);
        final InOrder inOrder = inOrder(rInstance, concreteListenerMock, storageMock);
        inOrder.verify(concreteListenerMock).preRemove(rInstance);
        inOrder.verify(rInstance).preRemove();
        inOrder.verify(storageMock).remove(rInstance.getUri(), OWLClassR.class, descriptor);
    }

    private <T> T createInstance(Class<T> cls) {
        return ReflectionUtils.instantiateUsingDefaultConstructor(metamodelMock.entity(cls).getInstantiableJavaType());
    }

    @Test
    public void postPersistEntityLifecycleListenerIsCalledAfterInstanceIsInsertedIntoStorage() {
        final URI rId = Generators.createIndividualIdentifier();
        final OWLClassR rInstance = spy(new OWLClassR());
        when(cloneBuilderMock.buildClone(eq(rInstance), any(CloneConfiguration.class))).thenReturn(rInstance);
        when(storageMock.generateIdentifier(metamodelMock.entity(OWLClassR.class))).thenReturn(rId);
        uow.registerNewObject(rInstance, descriptor);
        uow.commit();
        final InOrder inOrder = inOrder(rInstance, concreteListenerMock, storageMock);
        inOrder.verify(storageMock).persist(rId, rInstance, descriptor);
        inOrder.verify(concreteListenerMock).postPersist(rInstance);
        inOrder.verify(rInstance).postPersist();
    }

    @Test
    public void postRemoveEntityLifecycleListenerIsCalledAfterStorageRemoveOccurs() {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(createInstance(OWLClassR.class));
        rInstance.setUri(rOriginal.getUri());
        when(cloneBuilderMock.buildClone(eq(rOriginal), any())).thenReturn(rInstance);
        uow.registerExistingObject(rOriginal, descriptor);
        uow.removeObject(rInstance);
        final InOrder inOrder = inOrder(rInstance, concreteListenerMock, storageMock);
        inOrder.verify(storageMock).remove(rInstance.getUri(), OWLClassR.class, descriptor);
        inOrder.verify(concreteListenerMock).postRemove(rInstance);
        inOrder.verify(rInstance).postRemove();
    }

    @Test
    public void postLoadEntityLifecycleListenerIsCalledAfterInstanceIsLoadedIntoPersistenceContext() {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(createInstance(OWLClassR.class));
        rInstance.setUri(rOriginal.getUri());
        when(cloneBuilderMock.buildClone(eq(rOriginal), any())).thenReturn(rInstance);
        when(storageMock.find(new LoadingParameters<>(OWLClassR.class, rOriginal.getUri(), descriptor)))
                .thenReturn(rOriginal);
        final OWLClassR result = uow.readObject(OWLClassR.class, rOriginal.getUri(), descriptor);
        assertSame(rInstance, result);
        final InOrder inOrder = inOrder(rInstance, concreteListenerMock);
        inOrder.verify(concreteListenerMock).postLoad(rInstance);
        inOrder.verify(rInstance).postLoad();
    }

    @Test
    public void postLoadEntityLifecycleListenerIsCalledAfterInstanceRefresh() {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(createInstance(OWLClassR.class));
        rInstance.setUri(rOriginal.getUri());
        when(storageMock.find(any(LoadingParameters.class))).thenReturn(rOriginal);
        when(cloneBuilderMock.buildClone(eq(rOriginal), any())).thenReturn(rInstance);
        uow.registerExistingObject(rOriginal, descriptor);
        uow.refreshObject(rInstance);
        final InOrder inOrder = inOrder(rInstance, concreteListenerMock);
        inOrder.verify(concreteListenerMock).postLoad(rInstance);
        inOrder.verify(rInstance).postLoad();
    }

    @Test
    public void preUpdateIsCalledBeforeFieldUpdateIsMergedIntoStorage() throws Exception {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(createInstance(OWLClassR.class));
        rInstance.setUri(rOriginal.getUri());
        when(cloneBuilderMock.buildClone(eq(rOriginal), any())).thenReturn(rInstance);

        uow.registerExistingObject(rOriginal, descriptor);
        rInstance.setStringAtt("Update");
        // Manually trigger attribute change handling
        uow.attributeChanged(rInstance, OWLClassR.getStringAttField());
        final InOrder inOrder = inOrder(rInstance, concreteListenerMock, storageMock);
        inOrder.verify(concreteListenerMock).preUpdate(rInstance);
        inOrder.verify(rInstance).preUpdate();
        inOrder.verify(storageMock).merge(rInstance, mocks.forOwlClassR().rStringAtt(), descriptor);
    }

    @Test
    public void preUpdateIsCalledBeforeStorageMergeWhenDetachedInstanceIsMergedIntoPersistenceContext() {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rClone = spy(createInstance(OWLClassR.class));
        rClone.setUri(rOriginal.getUri());
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(storageMock.contains(rInstance.getUri(), rInstance.getClass(), descriptor)).thenReturn(true);
        when(storageMock.find(any())).thenReturn(rOriginal);
        rInstance.setStringAtt("differentString");
        when(cloneBuilderMock.buildClone(eq(rOriginal), any())).thenReturn(rClone);
        final OWLClassR merged = uow.mergeDetached(rInstance, descriptor);
        final InOrder inOrder = inOrder(merged, concreteListenerMock, storageMock);
        inOrder.verify(concreteListenerMock).preUpdate(merged);
        inOrder.verify(merged).preUpdate();
        inOrder.verify(storageMock, atLeastOnce()).merge(eq(merged), any(FieldSpecification.class), eq(descriptor));
    }

    @Test
    public void preUpdateIsNotCalledWhenMergedEntityHasNoChangesComparedToStorageOriginal() {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rClone = spy(createInstance(OWLClassR.class));
        rClone.setUri(rOriginal.getUri());
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(storageMock.contains(rInstance.getUri(), rInstance.getClass(), descriptor)).thenReturn(true);
        when(storageMock.find(any())).thenReturn(rOriginal);
        when(cloneBuilderMock.buildClone(eq(rOriginal), any())).thenReturn(rClone);
        final OWLClassR merged = uow.mergeDetached(rInstance, descriptor);
        verify(concreteListenerMock, never()).preUpdate(any());
        verify(merged, never()).preUpdate();
        verify(storageMock, never()).merge(eq(merged), any(FieldSpecification.class), eq(descriptor));
    }

    @Test
    public void postUpdateIsCalledAfterFieldUpdateWasMergedIntoStorage() throws Exception {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(createInstance(OWLClassR.class));
        rInstance.setUri(rOriginal.getUri());
        when(cloneBuilderMock.buildClone(eq(rOriginal), any())).thenReturn(rInstance);

        uow.registerExistingObject(rOriginal, descriptor);
        rInstance.setStringAtt("Update");
        // Manually trigger attribute change handling
        uow.attributeChanged(rInstance, OWLClassR.getStringAttField());
        final InOrder inOrder = inOrder(rInstance, concreteListenerMock, storageMock);
        inOrder.verify(storageMock).merge(rInstance, mocks.forOwlClassR().rStringAtt(), descriptor);
        inOrder.verify(concreteListenerMock).postUpdate(rInstance);
        inOrder.verify(rInstance).postUpdate();
    }

    @Test
    public void postUpdateIsCalledAfterStorageMergeWhenDetachedInstanceIsMergedIntoPersistenceContext() {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rClone = spy(createInstance(OWLClassR.class));
        rClone.setUri(rOriginal.getUri());
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(storageMock.contains(rInstance.getUri(), rInstance.getClass(), descriptor)).thenReturn(true);
        when(storageMock.find(any())).thenReturn(rOriginal);
        rInstance.setStringAtt("differentString");
        when(cloneBuilderMock.buildClone(eq(rOriginal), any())).thenReturn(rClone);
        final OWLClassR merged = uow.mergeDetached(rInstance, descriptor);
        final InOrder inOrder = inOrder(merged, concreteListenerMock, storageMock);
        inOrder.verify(storageMock, atLeastOnce()).merge(eq(merged), any(FieldSpecification.class), eq(descriptor));
        inOrder.verify(concreteListenerMock).postUpdate(merged);
        inOrder.verify(merged).postUpdate();
    }

    @Test
    public void postUpdateIsNotCalledWhenMergedEntityHasNoChangesComparedToStorageOriginal() {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rClone = spy(createInstance(OWLClassR.class));
        rClone.setUri(rOriginal.getUri());
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(storageMock.contains(rInstance.getUri(), rInstance.getClass(), descriptor)).thenReturn(true);
        when(storageMock.find(any())).thenReturn(rOriginal);
        when(cloneBuilderMock.buildClone(eq(rOriginal), any())).thenReturn(rClone);
        final OWLClassR merged = uow.mergeDetached(rInstance, descriptor);
        verify(concreteListenerMock, never()).postUpdate(any());
        verify(merged, never()).postUpdate();
        verify(storageMock, never()).merge(eq(merged), any(FieldSpecification.class), eq(descriptor));
    }

    @Test
    public void postLoadListenerMethodsAreCalledOnReferencedEntitiesAsWell() {
        final OWLClassR rOriginal = spy(new OWLClassR(Generators.createIndividualIdentifier()));
        final OWLClassR rInstance = spy(createInstance(OWLClassR.class));
        rInstance.setUri(rOriginal.getUri());
        final OWLClassA aOriginal = spy(Generators.generateOwlClassAInstance());
        final OWLClassA aInstance = spy(createInstance(OWLClassA.class));
        aInstance.setUri(aOriginal.getUri());
        rInstance.setOwlClassA(aInstance);

        when(cloneBuilderMock.buildClone(eq(rOriginal), any())).thenAnswer(inv -> {
            final CloneConfiguration config = (CloneConfiguration) inv.getArguments()[1];
            uow.registerExistingObject(aOriginal, config.getDescriptor(), config.getPostRegister());
            return rInstance;
        });
        when(cloneBuilderMock.buildClone(eq(aOriginal), any())).thenReturn(aInstance);
        when(storageMock.find(new LoadingParameters<>(OWLClassR.class, rOriginal.getUri(), descriptor)))
                .thenReturn(rOriginal);
        final OWLClassR result = uow.readObject(OWLClassR.class, rOriginal.getUri(), descriptor);
        assertSame(rInstance, result);
        final InOrder inOrder = inOrder(rInstance, aInstance, concreteListenerMock);
        inOrder.verify(aInstance).postLoad();
        inOrder.verify(concreteListenerMock).postLoad(rInstance);
        inOrder.verify(rInstance).postLoad();
    }

    @Test
    public void postLoadListenersAreCalledOnPluralReferencesAsWell() {
        final OWLClassC cOriginal = new OWLClassC(Generators.createIndividualIdentifier());
        final OWLClassC cInstance = createInstance(OWLClassC.class);
        cInstance.setUri(cOriginal.getUri());
        final OWLClassA aOriginal = spy(Generators.generateOwlClassAInstance());
        final OWLClassA aInstance = spy(createInstance(OWLClassA.class));
        aInstance.setUri(aOriginal.getUri());
        cOriginal.setSimpleList(Collections.singletonList(aOriginal));
        cInstance.setSimpleList(Collections.singletonList(aInstance));
        when(cloneBuilderMock.buildClone(eq(cOriginal), any())).thenAnswer(inv -> {
            final CloneConfiguration config = (CloneConfiguration) inv.getArguments()[1];
            uow.registerExistingObject(aOriginal, config.getDescriptor(), config.getPostRegister());
            return cInstance;
        });
        when(cloneBuilderMock.buildClone(eq(aOriginal), any())).thenReturn(aInstance);
        when(storageMock.find(new LoadingParameters<>(OWLClassC.class, cOriginal.getUri(), descriptor)))
                .thenReturn(cOriginal);
        final OWLClassC result = uow.readObject(OWLClassC.class, cOriginal.getUri(), descriptor);
        assertSame(cInstance, result);
        final InOrder inOrder = inOrder(aInstance, concreteListenerMock);
        inOrder.verify(aInstance).postLoad();
    }
}
