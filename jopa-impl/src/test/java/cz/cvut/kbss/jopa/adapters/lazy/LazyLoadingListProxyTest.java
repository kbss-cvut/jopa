package cz.cvut.kbss.jopa.adapters.lazy;

import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.utils.Generators;
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
import java.util.ListIterator;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class LazyLoadingListProxyTest {

    @Mock
    private UnitOfWork uow;

    @Mock
    private FieldSpecification<OWLClassP, List<URI>> att;

    private List<URI> proxiedList;

    private OWLClassP entity;

    private LazyLoadingListProxy<OWLClassP, URI> sut;

    @BeforeEach
    void setUp() {
        this.proxiedList = spy(new ArrayList<>(List.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier())));
        this.entity = new OWLClassP();
        entity.setUri(Generators.createIndividualIdentifier());
        this.sut = new LazyLoadingListProxy<>(entity, att, uow);
        entity.setSimpleList(sut);
        when(uow.loadEntityField(entity, att)).thenAnswer(inv -> {
            final OWLClassP owner = inv.getArgument(0);
            owner.setSimpleList(proxiedList);
            return proxiedList;
        });
    }

    @Test
    void sizeTriggersLazyLoadingAndReturnsLoadedListSize() {
        assertEquals(proxiedList.size(), sut.size());
        verify(uow).loadEntityField(entity, att);
        // One call is in the assertion above, the other is the one we expect to be invoked by the SUT
        verify(proxiedList, times(2)).size();
    }

    @Test
    void isEmptyTriggersLazyLoadingAndReturnsLoadedListIsEmptyResult() {
        assertEquals(proxiedList.isEmpty(), sut.isEmpty());
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).isEmpty();
    }

    @Test
    void containsTriggersLazyLoadingAndReturnsLoadedListContainsResult() {
        final URI arg = proxiedList.get(0);
        assertEquals(proxiedList.contains(arg), sut.contains(arg));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).contains(arg);
    }

    @Test
    void iteratorTriggersLazyLoadingAndReturnsLoadedListIterator() {
        final Iterator<URI> result = sut.iterator();
        assertNotNull(result);
        final Iterator<URI> expected = proxiedList.iterator();
        while (result.hasNext()) {
            assertTrue(expected.hasNext());
            assertEquals(expected.next(), result.next());
        }
        assertFalse(expected.hasNext());
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).iterator();
    }

    @Test
    void toArrayTriggersLazyLoadingAndReturnsLoadedListToArrayResult() {
        assertArrayEquals(proxiedList.toArray(), sut.toArray());
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).toArray();
    }

    @Test
    void toTypedArrayTriggersLazyLoadingAndReturnsLoadedListToTypedArrayResult() {
        assertArrayEquals(proxiedList.toArray(new URI[] {}), sut.toArray(new URI[] {}));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).toArray(any(URI[].class));
    }

    @Test
    void addTriggersLazyLoadingAddsElementToLoadedListAndReturnsResult() {
        final URI toAdd = Generators.createIndividualIdentifier();
        assertTrue(sut.add(toAdd));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList).add(toAdd);
    }

    @Test
    void removeTriggersLazyLoadingRemovesElementFromLoadedListAndReturnsResult() {
        final URI toRemove = proxiedList.get(1);
        assertTrue(sut.remove(toRemove));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList).remove(toRemove);
    }

    @Test
    void containsAllTriggersLazyLoadingAndReturnsLoadedListContainsAllResult() {
        final Collection<URI> notContained = Set.of(Generators.createIndividualIdentifier(), proxiedList.get(0));
        assertEquals(proxiedList.containsAll(notContained), sut.containsAll(notContained));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).containsAll(notContained);
    }

    @Test
    void addAllTriggersLazyLoadingAddsElementsToLoadedListAndReturnsResult() {
        final Collection<URI> toAdd = List.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier());
        assertTrue(sut.addAll(toAdd));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList).addAll(toAdd);
    }

    @Test
    void addAllAtIndexTriggersLazyLoadingAddsElementsToLoadedListAndReturnsResult() {
        final Collection<URI> toAdd = List.of(Generators.createIndividualIdentifier(), Generators.createIndividualIdentifier());
        final int index = proxiedList.size();
        assertTrue(sut.addAll(index, toAdd));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList).addAll(index, toAdd);
    }

    @Test
    void removeAllTriggersLazyLoadingRemovesElementsAndReturnsResult() {
        final Collection<URI> toRemove = new ArrayList<>(proxiedList);
        assertTrue(sut.removeAll(toRemove));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList).removeAll(toRemove);
    }

    @Test
    void retainAllTriggersLazyLoadingRetainsElementsAndReturnsResult() {
        final Collection<URI> toRetain = List.of(proxiedList.get(0));
        assertTrue(sut.retainAll(toRetain));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList).retainAll(toRetain);
    }

    @Test
    void clearTriggersLazyLoadingAndClearsLoadedList() {
        sut.clear();
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList).clear();
    }

    @Test
    void getTriggersLazyLoadingAndReturnsLoadedListGetResult() {
        final int index = 1;
        assertEquals(proxiedList.get(index), sut.get(index));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).get(index);
    }

    @Test
    void setTriggersLazyLoadingSetsValueInLoadedListAndReturnsResult() {
        final URI toSet = Generators.createIndividualIdentifier();
        final int index = 0;
        assertEquals(proxiedList.get(index), sut.set(index, toSet));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList).set(index, toSet);
    }

    @Test
    void addAtIndexTriggersLazyLoadingAndAddsElementToLoadedList() {
        final URI toAdd = Generators.createIndividualIdentifier();
        final int index = 1;
        sut.add(index, toAdd);
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList).add(index, toAdd);
    }

    @Test
    void removeAtIndexTriggersLazyLoadingRemovesElementFromLoadedListAndReturnsResult() {
        final int index = 0;
        assertEquals(proxiedList.get(index), sut.remove(index));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList).remove(index);
    }

    @Test
    void indexOfTriggersLazyLoadingAndReturnsLoadedListIndexOfResult() {
        final URI indexOfItem = proxiedList.get(1);
        assertEquals(proxiedList.indexOf(indexOfItem), sut.indexOf(indexOfItem));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).indexOf(indexOfItem);
    }

    @Test
    void lastIndexOfTriggersLazyLoadingAndReturnsLoadedListLastIndexOfResult() {
        final URI lastIndexOfItem = proxiedList.get(1);
        assertEquals(proxiedList.lastIndexOf(lastIndexOfItem), sut.lastIndexOf(lastIndexOfItem));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).lastIndexOf(lastIndexOfItem);
    }

    @Test
    void listIteratorTriggersLazyLoadingAndReturnsLoadedListListIterator() {
        final ListIterator<URI> result = sut.listIterator();
        assertNotNull(result);
        final ListIterator<URI> expected = proxiedList.listIterator();
        while (result.hasNext()) {
            assertTrue(expected.hasNext());
            assertEquals(expected.next(), result.next());
        }
        assertFalse(expected.hasNext());
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).listIterator();
    }

    @Test
    void listIteratorAtIndexTriggersLazyLoadingAndReturnsLoadedListListIterator() {
        final int index = 1;
        final ListIterator<URI> result = sut.listIterator(index);
        assertNotNull(result);
        final ListIterator<URI> expected = proxiedList.listIterator(index);
        while (result.hasNext()) {
            assertTrue(expected.hasNext());
            assertEquals(expected.next(), result.next());
        }
        assertFalse(expected.hasNext());
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).listIterator(index);
    }

    @Test
    void subListTriggersLazyLoadingAndReturnsLoadedListSubListResult() {
        final int start = 0;
        final int end = 1;
        assertEquals(proxiedList.subList(start, end), sut.subList(start, end));
        verify(uow).loadEntityField(entity, att);
        verify(proxiedList, times(2)).subList(start, end);
    }
}
