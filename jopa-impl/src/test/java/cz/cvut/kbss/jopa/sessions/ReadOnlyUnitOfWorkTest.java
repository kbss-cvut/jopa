package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingListProxy;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingSetProxy;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxy;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxyGenerator;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static cz.cvut.kbss.jopa.utils.EntityPropertiesUtils.getValueAsURI;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.never;
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
    void throwsUnsupportedOperationOnRegisterExistingObjectWithCloneDescriptor() {
        assertThrows(UnsupportedOperationException.class, () -> uow.registerExistingObject(objectMock, cloneRegistrationDescriptorMock));
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
    void throwsUnsupportedOperationOnGetCloneForOriginal() {
        assertThrows(UnsupportedOperationException.class, () -> uow.getCloneForOriginal(objectMock));
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
    void throwsUnsupportedOperationOnCommitToStorage() {
        assertThrows(UnsupportedOperationException.class, () -> uow.commitToStorage());
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
        when(storageMock.find(params)).thenReturn(entityA);
        defaultLoadStateDescriptor(entityA);

        uow.readObjectInternal(OWLClassA.class, entityA.getUri(), descriptor);

        // check object is managed
        assertTrue(uow.isObjectManaged(entityA));
    }

    @Test
    void readObjectInternalReturnsObject() {
        LoadingParameters<OWLClassA> params = new LoadingParameters<>(OWLClassA.class, getValueAsURI(entityA.getUri()), descriptor);
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
    void registerExistingObjectRegistersObjectPropertyField() {
        defaultLoadStateDescriptor(entityA, entityL);
        entityL.setSingleA(entityA);

        uow.registerExistingObject(entityL, descriptor);

        assertTrue(uow.isObjectManaged(entityA));
        assertTrue(uow.containsOriginal(entityA));
    }


    @Test
    @Override
    void testGetState() {
        fail("Not implemented yet");
    }

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void commitRemovesRemovedObjectsFromStorage() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void registerExistingObjectPassesPostCloneListenersToCloneBuilder() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void isObjectNewReturnsFalseForRegisteredExistingObject() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void registerNewObjectThrowsIdentifierNotSetExceptionWhenIdentifierIsNullAndNotGenerated() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void rollbackRollsBackStorageChangesAndRemovesObjectsFromPersistenceContext() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void commitPersistsAllNewlyRegisteredObjects() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void registerNewObjectGeneratesIdentifierWhenInstancesDoesNotHaveOne() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void commitAddsNewlyAddedReferenceToObjectToCache() throws Exception {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void readNewlyRegisteredObjectReturnsIt() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void registerNewObjectAddsArgumentToPersistenceContext() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void registerNewObjectThrowsNullPointerExceptionForNullDescriptor() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void removeObjectFromCacheEvictsObjectFromCacheManager() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void registerNewObjectThrowsNullPointerExceptionForNullArgument() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void unregisterObjectRemovesItFromCloneBuilderCache() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void persistSkipsCardinalityConstraintValidationOfInferredAttributes() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void commitAddsNewObjectsToCache() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void commitEvictsRemovedObjectsFromCache() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void isObjectNewReturnsFalseForNullArgument() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void isObjectNewReturnsTrueForNewlyRegisteredObject() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void isLoadedByAttributeReturnsLoadedForAttributesOfNewlyRegisteredInstance() throws Exception {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void getManagedOriginalReturnsNullWhenObjectIsManagedButAmongDeletedObjects() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void isLoadedReturnsLoadedForNewlyRegisteredInstance() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void removeNotRegisteredObjectThrowsIllegalArgumentException() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void restoreDeletedReinsertsObjectIntoRepository() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void commitDetachesPersistedInstance() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void removeObjectRemovesNewlyRegisteredObjectFromPersistenceContext() {}

    @Test
    @Disabled("Requires unsupported operation")
    @Override
    void restoreDeletedRegistersObjectAgain() {}
}
