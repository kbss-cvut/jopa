package cz.cvut.kbss.jopa.feature;

import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.environment.utils.TestEnvironmentUtils;
import cz.cvut.kbss.jopa.model.EntityManagerImpl;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.*;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;

import static org.junit.Assert.assertSame;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.*;

/**
 * Verifies entity lifecycle listener behavior w.r.t. the JPA 2.1 spec.
 */
public class EntityLifecycleListenersTest {

    private Descriptor descriptor;

    @Mock
    private MetamodelImpl metamodelMock;

    @Mock
    private CloneBuilderImpl cloneBuilderMock;

    @Mock
    private ConnectionWrapper storageMock;

    @Mock
    private EntityManagerImpl emMock;

    private UnitOfWorkImpl uow;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.descriptor = new EntityDescriptor();
        final ServerSessionStub serverSessionStub = spy(new ServerSessionStub(storageMock));
        when(serverSessionStub.getMetamodel()).thenReturn(metamodelMock);
        when(serverSessionStub.getLiveObjectCache()).thenReturn(mock(CacheManager.class));
        final EntityTransaction transactionMock = mock(EntityTransaction.class);
        when(emMock.getTransaction()).thenReturn(transactionMock);
        when(transactionMock.isActive()).thenReturn(true);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        uow = new UnitOfWorkImpl(serverSessionStub);
        uow.setEntityManager(emMock);
        TestEnvironmentUtils.setMock(uow, UnitOfWorkImpl.class.getDeclaredField("cloneBuilder"), cloneBuilderMock);
    }

    @Test
    public void prePersistLifecycleListenerIsCalledBeforeInstanceIsInsertedIntoPersistenceContext() {
        final OWLClassR rInstance = spy(new OWLClassR());
        uow.registerNewObject(rInstance, descriptor);
        final InOrder inOrder = inOrder(rInstance, storageMock);
        inOrder.verify(rInstance).prePersist();
        inOrder.verify(storageMock).persist(any(Object.class), eq(rInstance), eq(descriptor));
    }

    @Test
    public void preRemoveEntityLifecycleListenerIsCalledBeforeInstanceIsRemovedFromPersistenceContext() throws
                                                                                                        Exception {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(cloneBuilderMock.buildClone(rOriginal, descriptor)).thenReturn(rInstance);
        uow.registerExistingObject(rOriginal, descriptor);
        uow.removeObject(rInstance);
        final InOrder inOrder = inOrder(rInstance, storageMock);
        inOrder.verify(rInstance).preRemove();
        inOrder.verify(storageMock).remove(rInstance.getUri(), OWLClassR.class, descriptor);
    }

    @Test
    public void postPersistEntityLifecycleListenerIsCalledAfterStoragePersistOccurs() {
        final OWLClassR rInstance = spy(new OWLClassR());
        uow.registerNewObject(rInstance, descriptor);
        final InOrder inOrder = inOrder(rInstance, storageMock);
        inOrder.verify(storageMock).persist(any(Object.class), eq(rInstance), eq(descriptor));
        inOrder.verify(rInstance).postPersist();
    }

    @Test
    public void postRemoveEntityLifecycleListenerIsCalledAfterStorageRemoveOccurs() throws Exception {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(cloneBuilderMock.buildClone(rOriginal, descriptor)).thenReturn(rInstance);
        uow.registerExistingObject(rOriginal, descriptor);
        uow.removeObject(rInstance);
        final InOrder inOrder = inOrder(rInstance, storageMock);
        inOrder.verify(storageMock).remove(rInstance.getUri(), OWLClassR.class, descriptor);
        inOrder.verify(rInstance).postRemove();
    }

    @Test
    public void postLoadEntityLifecycleListenerIsCalledAfterInstanceIsLoadedIntoPersistenceContext() throws Exception {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(cloneBuilderMock.buildClone(rOriginal, descriptor)).thenReturn(rInstance);
        when(storageMock.find(new LoadingParameters<>(OWLClassR.class, rOriginal.getUri(), descriptor)))
                .thenReturn(rOriginal);
        final OWLClassR result = uow.readObject(OWLClassR.class, rOriginal.getUri(), descriptor);
        assertSame(rInstance, result);
        verify(rInstance).postLoad();
    }

    @Test
    public void postLoadEntityLifecycleListenerIsCalledAfterInstanceRefresh() throws Exception {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(cloneBuilderMock.buildClone(rOriginal, descriptor)).thenReturn(rInstance);
        uow.registerExistingObject(rOriginal, descriptor);
        uow.revertObject(rInstance);
        verify(rInstance).postLoad();
    }

    @Test
    public void preUpdateIsCalledBeforeFieldUpdateIsMergedIntoStorage() throws Exception {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(cloneBuilderMock.buildClone(rOriginal, descriptor)).thenReturn(rInstance);

        uow.registerExistingObject(rOriginal, descriptor);
        rInstance.setStringAtt("Update");
        // Have to call it manually, aspects do not work here
        uow.attributeChanged(rInstance, OWLClassR.getStringAttField());
        final InOrder inOrder = inOrder(rInstance, storageMock);
        inOrder.verify(rInstance).preUpdate();
        inOrder.verify(storageMock).merge(rInstance, OWLClassR.getStringAttField(), descriptor);
    }

    @Test
    public void preUpdateIsCalledBeforeStorageMergeWhenDetachedInstanceIsMergedIntoPersistenceContext() {
        final OWLClassR rOriginal = spy(new OWLClassR(Generators.createIndividualIdentifier()));
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(storageMock.contains(rInstance.getUri(), rInstance.getClass(), descriptor)).thenReturn(true);
        when(storageMock.find(new LoadingParameters<>(OWLClassR.class, rInstance.getUri(), descriptor, true)))
                .thenReturn(rOriginal);
        rInstance.setStringAtt("differentString");
        when(cloneBuilderMock.buildClone(rOriginal, descriptor)).thenReturn(rOriginal);
        final OWLClassR merged = uow.mergeDetached(rInstance, descriptor);
        final InOrder inOrder = inOrder(merged, storageMock);
        inOrder.verify(merged).preUpdate();
        inOrder.verify(storageMock, atLeastOnce()).merge(eq(merged), any(Field.class), eq(descriptor));
    }

    @Test
    public void preUpdateIsNotCalledWhenMergedEntityHasNoChangesComparedToStorageOriginal() {
        final OWLClassR rOriginal = spy(new OWLClassR(Generators.createIndividualIdentifier()));
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(storageMock.contains(rInstance.getUri(), rInstance.getClass(), descriptor)).thenReturn(true);
        when(storageMock.find(new LoadingParameters<>(OWLClassR.class, rInstance.getUri(), descriptor, true)))
                .thenReturn(rOriginal);
        when(cloneBuilderMock.buildClone(rOriginal, descriptor)).thenReturn(rOriginal);
        final OWLClassR merged = uow.mergeDetached(rInstance, descriptor);
        verify(merged, never()).preUpdate();
        verify(storageMock, never()).merge(eq(merged), any(Field.class), eq(descriptor));
    }

    @Test
    public void postUpdateIsCalledAfterFieldUpdateWasMergedIntoStorage() throws Exception {
        final OWLClassR rOriginal = new OWLClassR(Generators.createIndividualIdentifier());
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(cloneBuilderMock.buildClone(rOriginal, descriptor)).thenReturn(rInstance);

        uow.registerExistingObject(rOriginal, descriptor);
        rInstance.setStringAtt("Update");
        // Have to call it manually, aspects do not work here
        uow.attributeChanged(rInstance, OWLClassR.getStringAttField());
        final InOrder inOrder = inOrder(rInstance, storageMock);
        inOrder.verify(storageMock).merge(rInstance, OWLClassR.getStringAttField(), descriptor);
        inOrder.verify(rInstance).postUpdate();
    }

    @Test
    public void postUpdateIsCalledAfterStorageMergeWhenDetachedInstanceIsMergedIntoPersistenceContext() {
        final OWLClassR rOriginal = spy(new OWLClassR(Generators.createIndividualIdentifier()));
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(storageMock.contains(rInstance.getUri(), rInstance.getClass(), descriptor)).thenReturn(true);
        when(storageMock.find(new LoadingParameters<>(OWLClassR.class, rInstance.getUri(), descriptor, true)))
                .thenReturn(rOriginal);
        rInstance.setStringAtt("differentString");
        when(cloneBuilderMock.buildClone(rOriginal, descriptor)).thenReturn(rOriginal);
        final OWLClassR merged = uow.mergeDetached(rInstance, descriptor);
        final InOrder inOrder = inOrder(merged, storageMock);
        inOrder.verify(storageMock, atLeastOnce()).merge(eq(merged), any(Field.class), eq(descriptor));
        inOrder.verify(merged).postUpdate();
    }

    @Test
    public void postUpdateIsNotCalledWhenMergedEntityHasNoChangesComparedToStorageOriginal() {
        final OWLClassR rOriginal = spy(new OWLClassR(Generators.createIndividualIdentifier()));
        final OWLClassR rInstance = spy(new OWLClassR(rOriginal.getUri()));
        when(storageMock.contains(rInstance.getUri(), rInstance.getClass(), descriptor)).thenReturn(true);
        when(storageMock.find(new LoadingParameters<>(OWLClassR.class, rInstance.getUri(), descriptor, true)))
                .thenReturn(rOriginal);
        when(cloneBuilderMock.buildClone(rOriginal, descriptor)).thenReturn(rOriginal);
        final OWLClassR merged = uow.mergeDetached(rInstance, descriptor);
        verify(merged, never()).postUpdate();
        verify(storageMock, never()).merge(eq(merged), any(Field.class), eq(descriptor));
    }
}
