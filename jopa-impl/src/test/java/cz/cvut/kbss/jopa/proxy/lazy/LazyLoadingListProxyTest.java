package cz.cvut.kbss.jopa.proxy.lazy;

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
import java.util.List;
import java.util.ListIterator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
        when(uow.isActive()).thenReturn(true);
        when(uow.loadEntityField(entity, att)).thenAnswer(inv -> {
            final OWLClassP owner = inv.getArgument(0);
            owner.setSimpleList(proxiedList);
            return proxiedList;
        });
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
