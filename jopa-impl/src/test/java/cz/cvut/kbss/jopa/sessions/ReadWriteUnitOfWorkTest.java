package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassE;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exception.IdentifierNotSetException;
import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.exceptions.EntityNotFoundException;
import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.proxy.change.ChangeTrackingIndirectMultilingualString;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.sessions.cache.Descriptors;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import java.lang.annotation.Annotation;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static cz.cvut.kbss.jopa.environment.utils.ContainsSameEntities.containsSameEntities;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.not;
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


abstract class ReadWriteUnitOfWorkTest extends AbstractUnitOfWorkTestRunner {

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
    void commitRemovesRemovedObjectsFromStorage() {
        defaultLoadStateDescriptor(entityA, entityB);
        final Object toRemove = uow.registerExistingObject(entityA, descriptor);
        uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);
        uow.commit();

        verify(storageMock).remove(entityA.getUri(), entityA.getClass(), descriptor);
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
    void isObjectNewReturnsTrueForNewlyRegisteredObject() {
        final OWLClassA testNew = Generators.generateOwlClassAInstance();
        uow.registerNewObject(testNew, descriptor);
        assertTrue(uow.isObjectNew(testNew));
    }

    @Test
    void registerNewObjectThrowsIdentifierNotSetExceptionWhenIdentifierIsNullAndNotGenerated() {
        final OWLClassB b = new OWLClassB();
        assertThrows(IdentifierNotSetException.class, () -> uow.registerNewObject(b, descriptor));
        verify(storageMock, never()).persist(any(Object.class), any(Object.class), eq(descriptor));
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
    void readNewlyRegisteredObjectReturnsIt() {
        uow.registerNewObject(entityA, descriptor);
        assertTrue(uow.contains(entityA));
        final OWLClassA res = uow.readObject(OWLClassA.class, entityA.getUri(), descriptor);
        assertSame(entityA, res);
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
    void registerNewObjectThrowsNullPointerExceptionForNullDescriptor() {
        assertThrows(NullPointerException.class, () -> uow.registerNewObject(entityA, null));
    }

    @Test
    void registerNewObjectThrowsNullPointerExceptionForNullArgument() {
        assertThrows(NullPointerException.class, () -> uow.registerNewObject(null, descriptor));
    }

    @Test
    void unregisterObjectRemovesItFromCloneBuilderCache() {
        defaultLoadStateDescriptor(entityA);
        final OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        uow.unregisterObject(managed);
        verify(cloneBuilder).removeVisited(entityA, descriptor);
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
    void commitEvictsRemovedObjectsFromCache() {
        defaultLoadStateDescriptor(entityA, entityB);
        final Object toRemove = uow.registerExistingObject(entityA, descriptor);
        uow.registerExistingObject(entityB, descriptor);
        uow.removeObject(toRemove);
        uow.commit();

        verify(serverSessionStub.getLiveObjectCache()).evict(OWLClassA.class, entityA.getUri(), CONTEXT_URI);
    }

    @Test
    void isObjectNewReturnsFalseForNullArgument() {
        assertFalse(uow.isObjectNew(null));
    }

    @Test
    void isLoadedByAttributeReturnsLoadedForAttributesOfNewlyRegisteredInstance() throws Exception {
        uow.registerNewObject(entityA, descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(entityA, OWLClassA.getStrAttField().getName()));
        assertEquals(LoadState.LOADED, uow.isLoaded(entityA, OWLClassA.getTypesField().getName()));
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

    @Test
    void isLoadedReturnsLoadedForNewlyRegisteredInstance() {
        uow.registerNewObject(entityA, descriptor);
        assertEquals(LoadState.LOADED, uow.isLoaded(entityA));
    }

    @Test
    void removeNotRegisteredObjectThrowsIllegalArgumentException() {
        assertThrows(IllegalArgumentException.class, () -> uow.removeObject(entityA));
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
    void removeObjectRemovesNewlyRegisteredObjectFromPersistenceContext() {
        final OWLClassB newOne = new OWLClassB(Generators.createIndividualIdentifier());
        newOne.setStringAttribute("strAtt");
        uow.registerNewObject(newOne, descriptor);
        assertTrue(uow.contains(newOne));

        uow.removeObject(newOne);
        assertFalse(uow.contains(newOne));
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
    void refreshThrowsIllegalArgumentForRemovedInstance() {
        defaultLoadStateDescriptor(entityA);
        final Object a = uow.registerExistingObject(entityA, descriptor);
        uow.removeObject(a);
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class,
                () -> uow.refreshObject(a));
        assertEquals("Object not managed by this persistence context.", result.getMessage());
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
        assertThat(result.getMessage(), containsString(" no longer exists in the repository"));
    }

    @Test
    void refreshThrowsIllegalArgumentForNonManagedInstance() {
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class,
                () -> uow.refreshObject(
                        Generators.generateOwlClassAInstance()));
        assertEquals("Object not managed by this persistence context.", result.getMessage());
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
    void isObjectNewReturnsFalseForRegisteredExistingObject() {
        defaultLoadStateDescriptor(entityA);
        OWLClassA managed = (OWLClassA) uow.registerExistingObject(entityA, descriptor);
        assertFalse(uow.isObjectNew(managed));
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
    void removeObjectFromCacheEvictsObjectFromCacheManager() {
        uow.removeObjectFromCache(entityB, descriptor.getSingleContext().orElse(null));
        verify(serverSessionStub.getLiveObjectCache()).evict(OWLClassB.class, entityB.getUri(), descriptor.getSingleContext()
                                                                                                          .orElse(null));
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
    void commitEvictsInferredClassesFromCache() {
        defaultLoadStateDescriptor(entityA);
        uow.registerExistingObject(entityA, descriptor);
        uow.registerNewObject(entityB, descriptor);
        uow.commit();
        verify(serverSessionStub.getLiveObjectCache()).evictInferredObjects();
    }

    @Test
    void unregisterObjectReplacesChangeTrackingProxiesWithReferencedObjects() {
        when(transactionMock.isActive()).thenReturn(true);
        final OWLClassU entityU = new OWLClassU(Generators.createIndividualIdentifier());
        entityU.setSingularStringAtt(MultilingualString.create("test", "en"));
        defaultLoadStateDescriptor(entityU);
        final OWLClassU managed = (OWLClassU) uow.registerExistingObject(entityU, descriptor);
        assertInstanceOf(ChangeTrackingIndirectMultilingualString.class, managed.getSingularStringAtt());
        uow.unregisterObject(managed);
        assertThat(managed.getSingularStringAtt(), instanceOf(MultilingualString.class));
        assertThat(managed.getSingularStringAtt(), not(instanceOf(ChangeTrackingIndirectMultilingualString.class)));
    }
}
