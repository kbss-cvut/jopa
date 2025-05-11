package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.lifecycle.PostLoadInvoker;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingListProxy;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingSetProxy;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxy;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxyGenerator;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentMatchers;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

import static cz.cvut.kbss.jopa.utils.EntityPropertiesUtils.getValueAsURI;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ReadOnlyUnitOfWorkTest extends AbstractUnitOfWorkTestRunner {
    private Object objectMock;
    private Descriptor descriptorMock;
    private CloneRegistrationDescriptor cloneRegistrationDescriptorMock;
    private Object identifierMock;
    private Field fieldMock;
    private FieldSpecification fieldSpecificationMock;
    private ObjectChangeSet objectChangeSetMock;
    private URI uriMock;
    private ChangeRecord changeRecordMock;
    private EntityType entityTypeMock;

    @BeforeEach
    @Override
    protected void setUp() throws Exception {
        super.setUp();

        objectMock = Mockito.mock(Object.class);
        descriptorMock = Mockito.mock(Descriptor.class);
        cloneRegistrationDescriptorMock = Mockito.mock(CloneRegistrationDescriptor.class);
        identifierMock = Mockito.mock(Object.class);
        fieldMock = Mockito.mock(Field.class);
        fieldSpecificationMock = Mockito.mock(FieldSpecification.class);
        objectChangeSetMock = Mockito.mock(ObjectChangeSet.class);
        uriMock = Mockito.mock(URI.class);
        changeRecordMock = Mockito.mock(ChangeRecord.class);
        entityTypeMock = Mockito.mock(EntityType.class);
    }

    @Override
    protected AbstractUnitOfWork initUnitOfWork() {
        return new ReadOnlyUnitOfWork(serverSessionStub, new Configuration());
    }

    @AfterEach
    void tearDown() {
    }

    @Test
    void throwsUnsupportedOperationOnRemoveObject() {
        assertThrows(UnsupportedOperationException.class, () -> uow.removeObject(objectMock));
    }

    @Test
    void throwsUnsupportedOperationOnRegisterNewObject() {
        assertThrows(UnsupportedOperationException.class, () -> uow.registerNewObject(objectMock, descriptorMock));
    }

    @Test
    void throwsUnsupportedOperationOnMarkCloneForDeletion() {
        assertThrows(UnsupportedOperationException.class, () -> uow.markCloneForDeletion(objectMock, identifierMock));
    }

    @Test
    void throwsUnsupportedOperationOnAttributeChanged() {
        assertThrows(UnsupportedOperationException.class, () -> uow.attributeChanged(objectMock, fieldMock));
        assertThrows(UnsupportedOperationException.class, () -> uow.attributeChanged(objectMock, fieldSpecificationMock));
    }

    @Test
    void throwsUnsupportedOperationOnIsFlushingChanges() {
        assertThrows(UnsupportedOperationException.class, () -> uow.isFlushingChanges());
    }

    @Test
    void throwsUnsupportedOperationOnRestoreRemovedObject() {
        assertThrows(UnsupportedOperationException.class, () -> uow.restoreRemovedObject(objectMock));
    }

    @Test
    void throwsUnsupportedOperationOnHasChanges() {
        assertThrows(UnsupportedOperationException.class, () -> uow.hasChanges());
    }

    @Test
    void throwsUnsupportedOperationOnSetHasChanges() {
        assertThrows(UnsupportedOperationException.class, () -> uow.setHasChanges());
    }

    @Test
    void throwsUnsupportedOperationOnWriteUncommitedChanges() {
        assertThrows(UnsupportedOperationException.class, () -> uow.writeUncommittedChanges());
    }

    @Test
    void throwsUnsupportedOperationOnRegisterOriginalForNewClone() {
        assertThrows(UnsupportedOperationException.class, () -> uow.registerOriginalForNewClone(objectMock, objectMock));
    }

    @Test
    void throwsUnsupportedOperationOnRegisterClone() {
        assertThrows(UnsupportedOperationException.class, () -> uow.registerClone(objectMock, objectMock, descriptorMock));
    }

    @Test
    void throwsUnsupportedOperationOnPersistNewObjects() {
        assertThrows(UnsupportedOperationException.class, () -> uow.persistNewObjects());
    }

    @Test
    void throwsUnsupportedOperationOnCalculateChanges() {
        assertThrows(UnsupportedOperationException.class, () -> uow.calculateChanges());
    }

    @Test
    void throwsUnsupportedOperationOnValidateIntegrityConstraints() {
        assertThrows(UnsupportedOperationException.class, () -> uow.validateIntegrityConstraints());
    }

    @Test
    void throwsUnsupportedOperationOnProcessInferredValueChanges() {
        assertThrows(UnsupportedOperationException.class, () -> uow.processInferredValueChanges(objectChangeSetMock));
    }

    @Test
    void throwsUnsupportedOperationOnCopyChangeSet() {
        assertThrows(UnsupportedOperationException.class, () -> {
            ReadOnlyUnitOfWork.copyChangeSet(objectChangeSetMock, objectMock, objectMock, descriptorMock);
        });
    }

    @Test
    void throwsUnsupportedOperationOnRemoveObjectFromCache() {
        assertThrows(UnsupportedOperationException.class, () -> uow.removeObjectFromCache(objectMock, uriMock));
    }

    @Test
    void throwsUnsupportedOperationOnPreventCachingIfReferenceIsNotLoaded() {
        assertThrows(UnsupportedOperationException.class, () -> uow.preventCachingIfReferenceIsNotLoaded(changeRecordMock));
    }

    @Test
    void throwsUnsupportedOperationOnIsObjectNew() {
        assertThrows(UnsupportedOperationException.class, () -> uow.isObjectNew(objectMock));
    }

    @Test
    void throwsUnsupportedOperationOnMergeDetached() {
        assertThrows(UnsupportedOperationException.class, () -> uow.mergeDetached(entityA, descriptorMock));
    }

    @Test
    void throwsUnsupportedOperationOnMergeDetachedInternal() {
        assertThrows(UnsupportedOperationException.class, () -> uow.mergeDetachedInternal(entityA, descriptorMock));
    }

    @Test
    void throwsUnsupportedOperationOnGetInstanceForMerge() {
        assertThrows(UnsupportedOperationException.class, () -> uow.getInstanceForMerge(uriMock, entityTypeMock, descriptorMock));
    }

    @Test
    void throwsUnsupportedOperationOnEvictAfterMerge() {
        assertThrows(UnsupportedOperationException.class, () -> uow.evictAfterMerge(entityTypeMock, uriMock, descriptorMock));
    }

    @Test
    void throwsUnsupportedOperationOnRefreshObject() {
        assertThrows(UnsupportedOperationException.class, () -> uow.refreshObject(objectMock));
    }

    @Test
    @Override
    void getManagedOriginalReturnsManagedOriginalInstance() {
        LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor, false);
        params.bypassCache();
        when(storageMock.find(params)).thenReturn(
                entityA);
        defaultLoadStateDescriptor(entityA);
        uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);

        final OWLClassA res = uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertSame(entityA, res);
    }

    @Test
    @Override
    void getOriginalReturnsOriginalRegisteredByReadObject() {
        LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor, false);
        params.bypassCache();
        when(storageMock.find(params)).thenReturn(
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
    @Override
    void readObjectLoadsObjectFromStorage() {
        LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor, false);
        params.bypassCache();
        when(storageMock.find(params))
                .thenReturn(entityA);
        defaultLoadStateDescriptor(entityA);
        OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertNotNull(res);
        assertEquals(entityA.getUri(), res.getUri());
        verify(storageMock).find(params);
    }

    @Test
    @Override
    void clearCleansUpPersistenceContext() {
        final OWLClassD d = new OWLClassD();
        d.setUri(URI.create("http://dUri"));
        defaultLoadStateDescriptor(d);
        uow.registerExistingObject(d, descriptor);

        uow.clear();
        assertTrue(((ReadOnlyUnitOfWork) uow).keysToOriginals.isEmpty());
        assertTrue(((ReadOnlyUnitOfWork) uow).originalMapping.isEmpty());

        // these should be always empty in read-only uow
        assertTrue(uow.cloneToOriginals.isEmpty());
        assertTrue(uow.keysToClones.isEmpty());
        assertTrue(uow.deletedObjects.isEmpty());
        assertTrue(uow.newObjectsCloneToOriginal.isEmpty());
        assertTrue(uow.newObjectsKeyToClone.isEmpty());
    }

    @Test
    void registerExistingObjectReturnsRegisteredObject() {
        defaultLoadStateDescriptor(entityB);
        OWLClassB original = (OWLClassB) uow.registerExistingObject(entityB, descriptor);
        assertNotNull(original);
        assertEquals(entityB.getUri(), original.getUri());
        assertTrue(uow.contains(original));
        assertSame(entityB, uow.getOriginal(original));
    }

    @Test
    void readObjectInternalThrowsAssertionErrorOnNullParameter() {
        assertThrows(AssertionError.class, () -> uow.readObjectInternal(null, identifierMock, descriptorMock));
        assertThrows(AssertionError.class, () -> uow.readObjectInternal(objectMock.getClass(), null, descriptorMock));
        assertThrows(AssertionError.class, () -> uow.readObjectInternal(objectMock.getClass(), identifierMock, null));
    }

    @Test
    void readObjectInternalReturnsManagedObject() {
        defaultLoadStateDescriptor(entityA);
        uow.registerExistingObject(entityA, descriptor);
        OWLClassA result = uow.readObjectInternal(OWLClassA.class, entityA.getUri(), descriptor);
        assertSame(entityA, result);
    }

    @Test
    void readObjectInternalRegistersUnmanagedObject() {
        LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, getValueAsURI(entityA.getUri()), descriptor);
        params.bypassCache();
        when(storageMock.find(params)).thenReturn(entityA);
        defaultLoadStateDescriptor(entityA);

        uow.readObjectInternal(OWLClassA.class, entityA.getUri(), descriptor);

        // check object is managed
        assertTrue(uow.isObjectManaged(entityA));
    }

    @Test
    void readObjectInternalReturnsObject() {
        LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, getValueAsURI(entityA.getUri()), descriptor);
        params.bypassCache();
        when(storageMock.find(params)).thenReturn(entityA);
        defaultLoadStateDescriptor(entityA);

        OWLClassA result = uow.readObjectInternal(OWLClassA.class, entityA.getUri(), descriptor);

        // check object is managed
        assertSame(entityA, result);
    }

    @Test
    void readManagedObjectReturnsNullForUnmanagedObject() {
        OWLClassA result = uow.readManagedObject(OWLClassA.class, entityA.getUri(), descriptor);

        assertNull(result);
    }

    @Test
    void readManagedObjectReturnsRegisteredObject() {
        defaultLoadStateDescriptor(entityA);
        uow.registerExistingObject(entityA, descriptor);

        OWLClassA result = uow.readManagedObject(OWLClassA.class, entityA.getUri(), descriptor);

        assertSame(entityA, result);
    }

    @Test
    void getManagedOriginalReturnsNullForUnmanagedObject() {
        OWLClassA result = uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor);

        assertNull(result);
    }

    @Test
    void getManagedOriginalReturnsRegisteredObject() {
        defaultLoadStateDescriptor(entityA);
        uow.registerExistingObject(entityA, descriptor);

        OWLClassA result = uow.getManagedOriginal(OWLClassA.class, entityA.getUri(), descriptor);

        assertSame(entityA, result);
    }

    @Test
    void registerExistingObjectReturnsNullForNullObject() {
        Object result = uow.registerExistingObject(null, descriptorMock);
        assertNull(result);
    }

    @Test
    void registerExistingObjectRegistersUnmanagedObjectReturnsObject() {
        defaultLoadStateDescriptor(entityA);

        Object result = uow.registerExistingObject(entityA, descriptor);

        assertTrue(uow.containsOriginal(entityA));
        assertSame(entityA, result);
    }

    @Test
    void registerExistingObjectReturnsAlreadyRegisteredObject() {
        defaultLoadStateDescriptor(entityA);
        ((ReadOnlyUnitOfWork) uow).originalMapping.add(entityA);

        Object result = uow.registerExistingObject(entityA, descriptor);

        assertSame(entityA, result);
    }

    @Test
    void unregisterObjectDoesNotRemoveObjectForNull() {
        defaultLoadStateDescriptor(entityA);
        uow.registerExistingObject(entityA, descriptor);

        uow.unregisterObject(null);

        assertTrue(uow.containsOriginal(entityA));
    }

    @Test
    void unregisterObjectRemovesObjectFromPersistenceContext() {
        defaultLoadStateDescriptor(entityA);
        uow.registerExistingObject(entityA, descriptor);

        uow.unregisterObject(entityA);

        assertFalse(uow.containsOriginal(entityA));
        assertFalse(((ReadOnlyUnitOfWork) uow).keysToOriginals.containsKey(entityA.getUri()));
    }

    @Test
    void unregisterObjectUnregistersFromOntologyContext() {
        defaultLoadStateDescriptor(entityA);
        uow.registerExistingObject(entityA, descriptor);

        uow.unregisterObject(entityA);

        assertFalse(uow.isInRepository(descriptor, entityA));
    }

    @Test
    void registerExistingObjectProcessesLazyLoadingEntityProxyAttribute() {
        final LoadStateDescriptor<OWLClassL> loadStateDescriptor = LoadStateDescriptorFactory
                .createNotLoaded(entityL, metamodelMocks.forOwlClassL().entityType());
        uow.getLoadStateRegistry().put(entityL, loadStateDescriptor);
        when(metamodelMock.getLazyLoadingProxy(OWLClassA.class)).thenReturn((Class) new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class));

        final OWLClassL result = (OWLClassL) uow.registerExistingObject(entityL, descriptor);

        assertInstanceOf(LazyLoadingEntityProxy.class, result.getSingleA());
        assertInstanceOf(OWLClassA.class, result.getSingleA());
    }

    @Test
    void registerExistingObjectProcessesLazyLoadingListProxyAttribute() {
        final LoadStateDescriptor<OWLClassL> loadStateDescriptor = LoadStateDescriptorFactory
                .createNotLoaded(entityL, metamodelMocks.forOwlClassL().entityType());
        uow.getLoadStateRegistry().put(entityL, loadStateDescriptor);
        when(metamodelMock.getLazyLoadingProxy(OWLClassA.class)).thenReturn((Class) new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class));

        final OWLClassL result = (OWLClassL) uow.registerExistingObject(entityL, descriptor);

        assertInstanceOf(LazyLoadingListProxy.class, result.getSimpleList());
    }

    @Test
    void registerExistingObjectProcessesLazyLoadingSetProxyAttribute() {
        final LoadStateDescriptor<OWLClassL> loadStateDescriptor = LoadStateDescriptorFactory
                .createNotLoaded(entityL, metamodelMocks.forOwlClassL().entityType());
        uow.getLoadStateRegistry().put(entityL, loadStateDescriptor);
        when(metamodelMock.getLazyLoadingProxy(OWLClassA.class)).thenReturn((Class) new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class));

        final OWLClassL result = (OWLClassL) uow.registerExistingObject(entityL, descriptor);

        assertInstanceOf(LazyLoadingSetProxy.class, result.getSet());
    }

    @Test
    void registerExistingObjectProcessesNullAttribute() {
        defaultLoadStateDescriptor(entityA, entityL);

        final OWLClassL result = (OWLClassL) uow.registerExistingObject(entityL, descriptor);

        assertNull(result.getSingleA());
    }

    @Test
    void registerExistingObjectProcessesImmutableTypeField() {
        defaultLoadStateDescriptor(entityB);
        entityB.setStringAttribute("stringAttribute");

        final OWLClassB result = (OWLClassB) uow.registerExistingObject(entityB, descriptor);

        assertEquals("stringAttribute", result.getStringAttribute());
    }

    @Test
    void registerExistingObjectProcessesCollectionTypeField() {
        defaultLoadStateDescriptor(entityB);
        entityB.setProperties(Map.of("key", Set.of("val1", "val2")));

        final OWLClassB result = (OWLClassB) uow.registerExistingObject(entityB, descriptor);

        assertEquals(Map.of("key", Set.of("val1", "val2")), result.getProperties());
    }

    @Test
    void registerExistingObjectProcessesObjectPropertyField() {
        defaultLoadStateDescriptor(entityA, entityL);
        entityL.setSingleA(entityA);

        final OWLClassL result = (OWLClassL) uow.registerExistingObject(entityL, descriptor);

        assertSame(entityA, result.getSingleA());
    }

    @Test
    void registerExistingObjectRegistersObjectsInCollectionField() {
        defaultLoadStateDescriptor(entityA, entityL);
        entityL.setSimpleList(List.of(entityA, entityA));

        uow.registerExistingObject(entityL, descriptor);

        assertTrue(uow.isObjectManaged(entityA));
    }

    @Test
    void registerExistingObjectRegistersEagerlyLoadedEntityField() {
        defaultLoadStateDescriptor(entityD);

        uow.registerExistingObject(entityD, descriptor);

        assertTrue(uow.isObjectManaged(entityA));
    }

    @Test
    void getOriginalReturnsOriginal() {
        Object result = uow.getOriginal(entityA);

        assertSame(entityA, result);
    }

    @Test
    void getCloneForOriginalReturnsOriginal() {
        Object result = uow.getCloneForOriginal(entityA);

        assertSame(entityA, result);
    }

    @Test
    void containsOriginalThrowsAssertionErrorForNullArgument() {
        assertThrows(AssertionError.class, () -> uow.containsOriginal(null));
    }

    @Test
    void containsOriginalReturnsTrueForManagedEntity() {
        defaultLoadStateDescriptor(entityA);
        Object result = uow.registerExistingObject(entityA, descriptor);

        assertTrue(uow.containsOriginal(result));

    }

    @Test
    void containsOriginalReturnsFalseForNotManagedEntity() {
        defaultLoadStateDescriptor(entityA);

        assertFalse(uow.containsOriginal(entityA));
    }

    @Test
    void loadEntityFieldRegistersEntityAttribute() {
        final OWLClassL original = new OWLClassL(Generators.createIndividualIdentifier());
        final LoadStateDescriptor<OWLClassL> loadStateDescriptor = LoadStateDescriptorFactory
                .createNotLoaded(original, metamodelMocks.forOwlClassL().entityType());

        uow.getLoadStateRegistry().put(original, loadStateDescriptor);
        when(metamodelMock.getLazyLoadingProxy(OWLClassA.class)).thenReturn((Class) new LazyLoadingEntityProxyGenerator().generate(OWLClassA.class));
        final OWLClassL result = (OWLClassL) uow.registerExistingObject(original, descriptor);
        doAnswer(invocation -> {
            final FieldSpecification<?, ?> f = (FieldSpecification<?, ?>) invocation.getArguments()[1];
            EntityPropertiesUtils.setFieldValue(f.getJavaField(), invocation.getArguments()[0], entityA);
            return null;
        }).when(storageMock)
          .loadFieldValue(eq(result), eq(metamodelMocks.forOwlClassL().owlClassAAtt()), eq(descriptor));
        defaultLoadStateDescriptor(entityA);

        uow.loadEntityField(result, metamodelMocks.forOwlClassL().owlClassAAtt());

        assertNotNull(uow.getManagedOriginal(entityA.getClass(), entityA.getUri(), descriptor));
        assertEquals(LoadState.LOADED, uow.getLoadStateRegistry().get(original).isLoaded(metamodelMocks.forOwlClassL().owlClassAAtt()));
    }

    @Test
    void registerExistingObjectRegistersObjectPropertyField() {
        defaultLoadStateDescriptor(entityA, entityL);
        entityL.setSingleA(entityA);

        uow.registerExistingObject(entityL, descriptor);

        assertTrue(uow.isObjectManaged(entityA));
        assertTrue(uow.containsOriginal(entityA));
    }

    @Test
    void registerExistingObjectClonesOriginal() {
        defaultLoadStateDescriptor(entityD);

        OWLClassD clone = (OWLClassD) uow.registerExistingObject(entityD, new CloneRegistrationDescriptor(descriptor));

        // original on clone reference different instance
        assertNotSame(entityD, clone);

        // uri is cloned
        assertNotNull(clone.getUri());
        assertEquals(entityD.getUri(), clone.getUri());

        // object property is cloned
        assertNotNull(clone.getOwlClassA());
        assertNotSame(entityA, clone.getOwlClassA());
    }

    @Test
    void registerExistingObjectRegistersClonedOriginal() {
        defaultLoadStateDescriptor(entityD);

        OWLClassD clone = (OWLClassD) uow.registerExistingObject(entityD, new CloneRegistrationDescriptor(descriptor));

        // original is not registered
        assertFalse(uow.isObjectManaged(entityD));

        // clone is registered
        assertTrue(uow.isObjectManaged(clone));
    }

    @Test
    void isObjectManagedTrowsNullPointerExceptionForNullArgument() {
        assertThrows(NullPointerException.class, () -> uow.isObjectManaged(null));
    }

    @Test
    void isObjectManagedReturnsTrueForManagedObject() {
        defaultLoadStateDescriptor(entityA);
        Object result = uow.registerExistingObject(entityD, descriptor);

        assertTrue(uow.contains(result));
    }

    @Test
    void isObjectManagedReturnsFalseForNotManagedObject() {
        assertFalse(uow.isObjectManaged(objectMock));
    }

    @Test
    @Override
    void testGetState() {
        assertEquals(EntityState.NOT_MANAGED, uow.getState(entityA));
        defaultLoadStateDescriptor(entityA);
        OWLClassA result = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertEquals(EntityState.MANAGED, uow.getState(result));
    }

    @Test
    @Override
    void testGetStateWithDescriptor() {
        assertEquals(EntityState.NOT_MANAGED, uow.getState(entityA, descriptor));
        defaultLoadStateDescriptor(entityA);
        OWLClassA result = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertEquals(EntityState.MANAGED, uow.getState(result, descriptor));
    }

    @Test
    @Override
    void rollbackDetachesAllManagedEntities() {
        LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, entityA.getUri(), descriptor, false);
        params.bypassCache();
        when(storageMock.find(params))
                .thenReturn(entityA);
        defaultLoadStateDescriptor(entityA, entityB);
        final OWLClassA result = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        entityB.setProperties(new HashMap<>());
        uow.registerExistingObject(entityB, descriptor);
        uow.rollback();
        assertFalse(uow.contains(result));
        assertFalse(uow.contains(entityB));
    }

    @Test
    @Override
    void registerExistingObjectInvokesPostCloneListeners() {
        try (MockedConstruction<PostLoadInvoker> mockedConstruction = mockConstruction(PostLoadInvoker.class,
                (mock, context) -> {
                    doNothing().when(mock).accept(entityA);
                })) {
            defaultLoadStateDescriptor(entityA);

            final Object result = uow.registerExistingObject(entityA, descriptor);

            assertEquals(1, mockedConstruction.constructed().size(), "PostLoadInvoker was created exactly once");
            verify(mockedConstruction.constructed().get(0), times(1)).accept(result);
        }
    }

    @Test
    void registerExistingObjectWithCloningInvokesPostCloneListeners() {
        final Consumer<Object> plVerifier = mock(Consumer.class);
        defaultLoadStateDescriptor(entityA);
        final Object result = uow.registerExistingObject(entityA, new CloneRegistrationDescriptor(descriptor).postCloneHandlers(List.of(plVerifier)));
        verify(plVerifier).accept(result);
    }

    @Test
    void readObjectInternalClonesCachedEntity() {
        when(uow.getLiveObjectCache().contains(
                ArgumentMatchers.any(),
                ArgumentMatchers.any(),
                ArgumentMatchers.any()
        )).thenReturn(true);
        when(storageMock.find(ArgumentMatchers.any())).thenReturn(entityA);

        OWLClassA result = uow.readObjectInternal(entityA.getClass(), entityA.getUri(), descriptor);

        assertNotNull(result);
        assertNotSame(entityA, result);
    }
}
