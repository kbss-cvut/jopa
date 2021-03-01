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

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassF;

class IndirectSetTest {

    private static Set<OWLClassA> set;
    private static Set<OWLClassA> backupSet;
    private static OWLClassF owner;
    private static Field ownerField;

    private IndirectSet<OWLClassA> target;

    @Mock
    private UnitOfWorkImpl uow;

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
        MockitoAnnotations.openMocks(this);
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        target = new IndirectSet<>(owner, ownerField, uow, set);
        set.clear();
        set.addAll(backupSet);
        owner.setSimpleSet(target);
    }

    @Test
    void testIndirectSetNullUoW() {
        assertThrows(NullPointerException.class,() ->  new IndirectSet<>(owner, ownerField, null, set));
    }

    @Test
    void testIndirectSetNullReferencedSet() {
        assertThrows(NullPointerException.class,() ->  new IndirectSet<>(owner, ownerField, uow, null));
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
        final Iterator<OWLClassA> it = target.iterator();
        assertTrue(it.hasNext());
        it.next();
        it.remove();
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupSet.size() - 1, set.size());
    }

    @Test
    void testAdd() {
        final OWLClassA a = new OWLClassA();
        a.setUri(URI.create("http://newA"));
        a.setStringAttribute("testAttribute");
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
        final OWLClassA toRemove = set.iterator().next();
        target.remove(toRemove);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupSet.size() - 1, set.size());
    }

    @Test
    void testAddAll() {
        final List<OWLClassA> toAdd = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create("http://addAllA" + i));
            toAdd.add(a);
        }
        target.addAll(toAdd);
        verify(uow).attributeChanged(owner, ownerField);
        assertEquals(backupSet.size() + toAdd.size(), set.size());
        assertTrue(set.contains(toAdd.iterator().next()));
    }

    @Test
    void testRetainAll() {
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
