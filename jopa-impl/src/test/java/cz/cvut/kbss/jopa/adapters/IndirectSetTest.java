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
package cz.cvut.kbss.jopa.adapters;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassF;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class IndirectSetTest {

    private static Set<OWLClassA> set;
    private static Set<OWLClassA> backupSet;
    private static OWLClassF owner;
    private static Field ownerField;

    private IndirectSet<OWLClassA> target;

    @Mock
    private UnitOfWork uow;

    @BeforeAll
    static void setUpBeforeClass() throws Exception {
        owner = new OWLClassF();
        owner.setUri(URI.create("http://C"));
        ownerField = OWLClassF.class.getDeclaredField("simpleSet");
        backupSet = new HashSet<>(Generators.generateInstances(10));
        set = new HashSet<>();
        set.addAll(backupSet);
    }

    @BeforeEach
    void setUp() {
        target = new IndirectSet<>(owner, ownerField, uow, set);
        set.clear();
        set.addAll(backupSet);
        owner.setSimpleSet(target);
    }

    @Test
    void testIndirectSetNullUoW() {
        assertThrows(NullPointerException.class, () -> new IndirectSet<>(owner, ownerField, null, set));
    }

    @Test
    void testIndirectSetNullReferencedSet() {
        assertThrows(NullPointerException.class, () -> new IndirectSet<>(owner, ownerField, uow, null));
    }

    @Test
    void testContains() {
        final OWLClassA elem = backupSet.iterator().next();
        assertTrue(target.contains(elem));
    }

    @Test
    void testIteratorHasNext() {
        final Iterator<OWLClassA> it = set.iterator();
        final Iterator<OWLClassA> indIt = target.iterator();
        while (it.hasNext()) {
            it.next();
            assertTrue(indIt.hasNext());
            assertNotNull(indIt.next());
        }
    }

    @Test
    void testIteratorNext() {
        final Iterator<OWLClassA> it = set.iterator();
        final Iterator<OWLClassA> indIt = target.iterator();
        while (it.hasNext()) {
            assertTrue(indIt.hasNext());
            assertEquals(it.next(), indIt.next());
        }
    }

    @Test
    void testIteratorRemove() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        final Iterator<OWLClassA> it = target.iterator();
        assertTrue(it.hasNext());
        it.next();
        it.remove();
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupSet.size() - 1, set.size());
    }

    @Test
    void testAdd() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        final OWLClassA a = Generators.generateOwlClassAInstance();
        target.add(a);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupSet.size() + 1, set.size());
    }

    @Test
    void addingExistingElementDoesNotTriggerAttributeChange() {
        final OWLClassA toAdd = backupSet.iterator().next();
        target.add(toAdd);
        verify(uow, never()).attributeChanged(owner, ownerField);
    }

    @Test
    void testRemove() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        final OWLClassA toRemove = set.iterator().next();
        target.remove(toRemove);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupSet.size() - 1, set.size());
    }

    @Test
    void testAddAll() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        final List<OWLClassA> toAdd = IntStream.range(0, 5).mapToObj(i -> Generators.generateOwlClassAInstance())
                .collect(Collectors.toList());
        target.addAll(toAdd);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupSet.size() + toAdd.size(), set.size());
        assertTrue(set.contains(toAdd.iterator().next()));
    }

    @Test
    void testRetainAll() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        Set<OWLClassA> toRetain = new HashSet<>();
        Iterator<OWLClassA> it = backupSet.iterator();
        for (int i = 0; i < 8; i++) {
            assertTrue(it.hasNext());
            toRetain.add(it.next());
        }
        target.retainAll(toRetain);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(toRetain.size(), set.size());
        assertEquals(toRetain.size(), target.size());
    }

    @Test
    void testRemoveAll() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        Set<OWLClassA> toRemove = new HashSet<>();
        Iterator<OWLClassA> it = backupSet.iterator();
        for (int i = 0; i < 8; i++) {
            assertTrue(it.hasNext());
            toRemove.add(it.next());
        }
        target.removeAll(toRemove);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupSet.size() - toRemove.size(), set.size());
    }

    @Test
    void testRemoveAllNull() {
        assertThrows(NullPointerException.class, () -> target.removeAll(null));
    }

    @Test
    void testClear() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        target.clear();
        verify(uow).attributeChanged(owner, ownerField);
        assertTrue(set.isEmpty());
        assertTrue(target.isEmpty());
    }

    @Test
    void equalsWorksForTwoIndirectSets() {
        final IndirectSet<OWLClassA> other = new IndirectSet<>(owner, ownerField, uow, set);
        assertEquals(target, other);
    }

    @Test
    void equalsWorksForRegularSetAndIndirectSet() {
        assertEquals(backupSet, target);
    }

    @Test
    void equalsWorksForIndirectSetAndRegularSet() {
        assertEquals(target, backupSet);
    }

    @Test
    void hashCodeReturnsInternalSetHashCode() {
        assertEquals(backupSet.hashCode(), target.hashCode());
    }

    @Test
    void equalsReturnsFalseForInEqualIndirectSets() {
        set.add(Generators.generateOwlClassAInstance());
        assertNotEquals(backupSet, target);
    }

    @Test
    void containsAllInvokesInternalSet() {
        assertTrue(target.containsAll(backupSet));
        assertTrue(set.containsAll(backupSet));
    }
}
