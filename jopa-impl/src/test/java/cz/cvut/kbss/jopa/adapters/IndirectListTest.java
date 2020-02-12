/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.adapters;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class IndirectListTest {

    private static List<OWLClassA> list;
    private static List<OWLClassA> backupList;
    private static OWLClassC owner;
    private static Field ownerField;

    @Mock
    private UnitOfWorkImpl uow;

    private IndirectList<OWLClassA> target;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        owner = new OWLClassC();
        owner.setUri(URI.create("http://C"));
        ownerField = OWLClassC.class.getDeclaredField("referencedList");
        backupList = Generators.generateInstances(10);
        list = new ArrayList<>();
        list.addAll(backupList);
    }

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        target = new IndirectList<>(owner, ownerField, uow, list);
        list.clear();
        list.addAll(backupList);
        owner.setReferencedList(target);
    }

    @Test(expected = NullPointerException.class)
    public void testConstructorNull() {
        new IndirectList<>(owner, ownerField, uow, null);
    }

    @Test(expected = NullPointerException.class)
    public void testConstructorNullTwo() {
        new IndirectList<>(owner, ownerField, null, list);
    }

    @Test
    public void testAdd() {
        final OWLClassA added = new OWLClassA();
        added.setUri(URI.create("http://added"));
        owner.getReferencedList().add(added);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupList.size() + 1, target.size());
    }

    @Test
    public void testAddNull() {
        // Adding null is possible for some list types (ArrayList in our case
        // permits them)
        owner.getReferencedList().add(null);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupList.size() + 1, target.size());
    }

    @Test
    public void testAddAtIndex() {
        final OWLClassA added = new OWLClassA();
        added.setUri(URI.create("http://added"));
        owner.getReferencedList().add(list.size() / 2, added);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupList.size() + 1, target.size());
    }

    @Test
    public void testAddAll() {
        final List<OWLClassA> toAdd = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create("http://addedA" + i));
            toAdd.add(a);
        }
        owner.getReferencedList().addAll(toAdd);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupList.size() + toAdd.size(), target.size());
    }

    @Test
    public void addAllDoesNothingWhenNoNewElementsAreAdded() {
        owner.getReferencedList().addAll(Collections.emptyList());
        verify(uow, never()).attributeChanged(owner, ownerField);
    }

    @Test
    public void testAddAllAtIndex() {
        final List<OWLClassA> toAdd = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create("http://addedA" + i));
            toAdd.add(a);
        }
        final int index = 4;
        owner.getReferencedList().addAll(index, toAdd);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupList.size() + toAdd.size(), target.size());
        assertEquals(toAdd.get(0), target.get(index));
    }

    @Test
    public void addAllAtIndexDoesNothingWhenNoElementsAreAdded() {
        final int origSize = target.size();
        owner.getReferencedList().addAll(2, Collections.emptyList());
        verify(uow, never()).attributeChanged(owner, ownerField);
        assertEquals(origSize, target.size());
    }

    @Test
    public void testClear() {
        owner.getReferencedList().clear();
        verify(uow).attributeChanged(owner, ownerField);
        assertTrue(target.isEmpty());
    }

    @Test
    public void testRemoveObject() {
        owner.getReferencedList().remove(list.get(0));
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupList.size() - 1, target.size());
    }

    @Test
    public void testRemoveNull() {
        owner.getReferencedList().remove(null);
        verify(uow, never()).attributeChanged(any(), any(Field.class));
    }

    @Test
    public void testRemoveAtIndex() {
        owner.getReferencedList().remove(list.size() - 1);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupList.size() - 1, target.size());
    }

    @Test
    public void testRemoveAll() {
        List<OWLClassA> toRemove = list.subList(2, 5);
        final int toRemoveSize = toRemove.size();
        owner.getReferencedList().removeAll(toRemove);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupList.size() - toRemoveSize, target.size());
    }

    @Test
    public void removeAllDoesNothingWhenNoElementsAreRemoved() {
        owner.getReferencedList().removeAll(Collections.emptyList());
        verify(uow, never()).attributeChanged(owner, ownerField);
    }

    @Test
    public void testRetainAll() {
        List<OWLClassA> toRetain = list.subList(2, 5);
        final int toRetainSize = toRetain.size();
        owner.getReferencedList().retainAll(toRetain);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(toRetainSize, target.size());
    }

    @Test
    public void testSet() {
        final OWLClassA a = new OWLClassA();
        a.setUri(URI.create("http://setA"));
        owner.getReferencedList().set(0, a);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupList.size(), target.size());
        assertTrue(target.contains(a));
    }

    @Test
    public void testIteratorRemove() {
        int cnt = 3;
        final OWLClassA toRemove = owner.getReferencedList().get(cnt);
        final Iterator<OWLClassA> it = owner.getReferencedList().iterator();
        int i = 0;
        while (it.hasNext() && i < cnt + 1) {
            i++;
            it.next();
        }
        it.remove();
        verify(uow).attributeChanged(owner, ownerField);
        assertFalse(owner.getReferencedList().contains(toRemove));
    }

    @Test
    public void testListIteratorAdd() {
        final OWLClassA addA = new OWLClassA();
        addA.setUri(URI.create("http://addA"));
        final ListIterator<OWLClassA> lit = owner.getReferencedList().listIterator();
        int i = 0;
        int pos = 4;
        while (lit.hasNext() && i < pos) {
            i++;
            lit.next();
        }
        lit.add(addA);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(owner.getReferencedList().get(pos), addA);
    }

    @Test
    public void testListIteratorSetReverse() {
        final OWLClassA a = new OWLClassA();
        a.setUri(URI.create("http://setA"));
        final ListIterator<OWLClassA> lit = owner.getReferencedList().listIterator(5);
        while (lit.hasPrevious()) {
            lit.previous();
        }
        lit.set(a);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(owner.getReferencedList().get(0), a);
    }

    @Test
    public void testListIteratorRemove() {
        int cnt = 3;
        final OWLClassA toRemove = owner.getReferencedList().get(cnt);
        final ListIterator<OWLClassA> it = owner.getReferencedList().listIterator();
        int i = 0;
        while (it.hasNext() && i < cnt + 1) {
            i++;
            it.next();
        }
        it.remove();
        verify(uow).attributeChanged(owner, ownerField);
        assertFalse(owner.getReferencedList().contains(toRemove));
        assertEquals(cnt - 1, it.previousIndex());
        assertEquals(cnt, it.nextIndex());
    }

    @Test
    public void equalsWorksForTwoIndirectLists() {
        final IndirectList<OWLClassA> other = new IndirectList<>(owner, ownerField, uow, list);
        assertEquals(target, other);
    }

    @Test
    public void equalsWorksForIndirectListAndRegularList() {
        assertEquals(backupList, target);
    }

    @Test
    public void hashCodeIsEqualToRegularListWithIdenticalContent() {
        assertEquals(list.hashCode(), target.hashCode());
    }

    @Test
    public void testContainsAll() {
        assertTrue(target.containsAll(list));
    }

    @Test
    public void sublistReturnsAnotherIndirectList() {
        final List<OWLClassA> result = target.subList(0, target.size() / 2);
        assertTrue(result instanceof IndirectList);
    }

    @Test
    public void removeIfNotifiesUoWOfRemovedElements() {
        final Set<URI> toRemove = target.stream().filter(e -> Generators.randomBoolean()).map(OWLClassA::getUri)
                                        .collect(Collectors.toSet());
        owner.getReferencedList().removeIf(e -> toRemove.contains(e.getUri()));
        verify(uow, times(toRemove.size())).attributeChanged(owner, ownerField);
    }
}
