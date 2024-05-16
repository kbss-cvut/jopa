package cz.cvut.kbss.jopa.proxy.lazy;

import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.exception.LazyLoadingException;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class LazyLoadingCollectionProxyTest {

    @Mock
    private UnitOfWork uow;

    @Mock
    private FieldSpecification<OWLClassP, List<URI>> att;

    private List<URI> proxiedCollection;

    private OWLClassP entity;

    private LazyLoadingCollectionProxy<OWLClassP, List<URI>, URI> sut;

    @BeforeEach
    void setUp() {
        this.proxiedCollection = spy(new ArrayList<>(List.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier())));
        this.entity = new OWLClassP();
        entity.setUri(Generators.createIndividualIdentifier());
        this.sut = new LazyLoadingListProxy<>(entity, att, uow);
    }

    @Test
    void triggerLazyLoadingThrowsLazyLoadingExceptionWhenNoPersistenceContextIsAvailable() {
        this.sut = new LazyLoadingListProxy<>(entity, att, null);
        assertThrows(LazyLoadingException.class, () -> sut.triggerLazyLoading());
    }

    @Test
    void triggerLazyLoadingThrowsLazyLoadingExceptionWhenPersistenceContextIsNotActiveAnymore() {
        when(uow.isActive()).thenReturn(false);
        assertThrows(LazyLoadingException.class, () -> sut.triggerLazyLoading());
    }

    @Test
    void sizeTriggersLazyLoadingAndReturnsLoadedCollectionSize() {
        initLazyLoading();
        assertEquals(proxiedCollection.size(), sut.size());
        verify(uow).loadEntityField(entity, att);
        // One call is in the assertion above, the other is the one we expect to be invoked by the SUT
        verify(proxiedCollection, times(2)).size();
    }

    private void initLazyLoading() {
        entity.setSimpleList((List<URI>) sut);
        when(uow.isActive()).thenReturn(true);
        when(uow.loadEntityField(entity, att)).thenAnswer(inv -> {
            final OWLClassP owner = inv.getArgument(0);
            owner.setSimpleList(proxiedCollection);
            return proxiedCollection;
        });
    }

    @Test
    void isEmptyTriggersLazyLoadingAndReturnsLoadedCollectionIsEmptyResult() {
        initLazyLoading();
        assertEquals(proxiedCollection.isEmpty(), sut.isEmpty());
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection, times(2)).isEmpty();
    }

    @Test
    void containsTriggersLazyLoadingAndReturnsLoadedCollectionContainsResult() {
        initLazyLoading();
        final URI arg = proxiedCollection.get(0);
        assertEquals(proxiedCollection.contains(arg), sut.contains(arg));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection, times(2)).contains(arg);
    }

    @Test
    void iteratorTriggersLazyLoadingAndReturnsLoadedCollectionIterator() {
        initLazyLoading();
        final Iterator<URI> result = sut.iterator();
        assertNotNull(result);
        final Iterator<URI> expected = proxiedCollection.iterator();
        while (result.hasNext()) {
            assertTrue(expected.hasNext());
            assertEquals(expected.next(), result.next());
        }
        assertFalse(expected.hasNext());
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection, times(2)).iterator();
    }

    @Test
    void toArrayTriggersLazyLoadingAndReturnsLoadedCollectionToArrayResult() {
        initLazyLoading();
        assertArrayEquals(proxiedCollection.toArray(), sut.toArray());
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection, times(2)).toArray();
    }

    @Test
    void toTypedArrayTriggersLazyLoadingAndReturnsLoadedCollectionToTypedArrayResult() {
        initLazyLoading();
        assertArrayEquals(proxiedCollection.toArray(new URI[] {}), sut.toArray(new URI[] {}));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection, times(2)).toArray(any(URI[].class));
    }

    @Test
    void addTriggersLazyLoadingAddsElementToLoadedCollectionAndReturnsResult() {
        initLazyLoading();
        final URI toAdd = Generators.createIndividualIdentifier();
        assertTrue(sut.add(toAdd));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection).add(toAdd);
    }

    @Test
    void removeTriggersLazyLoadingRemovesElementFromLoadedCollectionAndReturnsResult() {
        initLazyLoading();
        final URI toRemove = proxiedCollection.get(1);
        assertTrue(sut.remove(toRemove));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection).remove(toRemove);
    }

    @Test
    void containsAllTriggersLazyLoadingAndReturnsLoadedCollectionContainsAllResult() {
        initLazyLoading();
        final Collection<URI> notContained = Set.of(Generators.createIndividualIdentifier(), proxiedCollection.get(0));
        assertEquals(proxiedCollection.containsAll(notContained), sut.containsAll(notContained));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection, times(2)).containsAll(notContained);
    }

    @Test
    void addAllTriggersLazyLoadingAddsElementsToLoadedCollectionAndReturnsResult() {
        initLazyLoading();
        final Collection<URI> toAdd = List.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier());
        assertTrue(sut.addAll(toAdd));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection).addAll(toAdd);
    }

    @Test
    void removeAllTriggersLazyLoadingRemovesElementsAndReturnsResult() {
        initLazyLoading();
        final Collection<URI> toRemove = new ArrayList<>(proxiedCollection);
        assertTrue(sut.removeAll(toRemove));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection).removeAll(toRemove);
    }

    @Test
    void retainAllTriggersLazyLoadingRetainsElementsAndReturnsResult() {
        initLazyLoading();
        final Collection<URI> toRetain = List.of(proxiedCollection.get(0));
        assertTrue(sut.retainAll(toRetain));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection).retainAll(toRetain);
    }

    @Test
    void clearTriggersLazyLoadingAndClearsLoadedCollection() {
        initLazyLoading();
        sut.clear();
        verify(uow).loadEntityField(entity, att);
        verify(proxiedCollection).clear();
    }

    @Test
    void getLoadedValueReturnsLoadedValueAfterTriggeringLazyLoading() {
        initLazyLoading();
        assertFalse(sut.isEmpty());
        assertTrue(sut.isLoaded());
        assertEquals(proxiedCollection, sut.getLoadedValue());
    }

    @Test
    void triggerLazyLoadingMultipleTimesReturnsAlreadyLoadedValueAndDoesNotCallPersistenceContext() {
        initLazyLoading();
        IntStream.range(0, 5).forEach(i -> assertEquals(proxiedCollection, sut.triggerLazyLoading()));
        verify(uow, times(1)).loadEntityField(entity, att);
    }

    @Test
    void getLoadedThrowsIllegalStateExceptionWhenCalledBeforeLoading() {
        assertThrows(IllegalStateException.class, () -> sut.getLoadedValue());
    }
}
