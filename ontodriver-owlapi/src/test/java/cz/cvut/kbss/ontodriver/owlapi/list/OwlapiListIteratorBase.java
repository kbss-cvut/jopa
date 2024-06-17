/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.change.TransactionalChange;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

abstract class OwlapiListIteratorBase {

    static final String SUBJECT = ListTestHelper.SUBJECT.getIdentifier().toString();

    OntologySnapshot snapshot;
    ListTestHelper testHelper;
    AxiomAdapter axiomAdapter;

    OwlapiListIterator<NamedResource> iterator;

    public void setUp() throws Exception {
        this.snapshot = TestUtils.initRealOntology(null);
        this.axiomAdapter = new AxiomAdapter(snapshot.getDataFactory());
    }

    abstract OwlapiListIterator<NamedResource> iterator();

    @Test
    public void nextWithoutMoreElementsThrowsException() {
        testHelper.persistList(ListTestHelper.LIST_ITEMS);
        while (iterator.hasNext()) {
            iterator.next();
        }
        assertThrows(NoSuchElementException.class, () -> iterator.next());
    }

    @Test
    public void testBasicIteration() {
        testHelper.persistList(ListTestHelper.LIST_ITEMS);
        verifyIterationContent(iterator, ListTestHelper.LIST_ITEMS);
    }

    private void verifyIterationContent(OwlapiListIterator<NamedResource> it, List<URI> expected) {
        int i = 0;
        while (it.hasNext()) {
            final Axiom<NamedResource> item = it.next();
            assertEquals(expected.get(i), item.getValue().getValue().getIdentifier());
            i++;
        }
        assertEquals(expected.size(), i);
    }

    @Test
    public void removeWithoutReconnectOnAllElementsClearsTheWholeList() {
        testHelper.persistList(ListTestHelper.LIST_ITEMS);
        final List<TransactionalChange> changes = new ArrayList<>();
        while (iterator.hasNext()) {
            iterator.next();
            changes.addAll(iterator.removeWithoutReconnect());
        }
        applyChanges(changes);
        final OwlapiListIterator<NamedResource> it = iterator();
        assertFalse(it.hasNext());
    }

    protected void applyChanges(List<TransactionalChange> changes) {
        final List<OWLOntologyChange> toApply = changes.stream()
                                                       .flatMap(o -> o.toOwlChanges(snapshot.getOntology()).stream())
                                                       .collect(Collectors.toList());
        snapshot.getOntologyManager().applyChanges(toApply);
    }

    @Test
    public void removeWithoutReconnectClearsRestOfList() {
        testHelper.persistList(ListTestHelper.LIST_ITEMS);
        final List<TransactionalChange> changes = new ArrayList<>();
        int i = 0;
        while (iterator.hasNext()) {
            iterator.next();
            if (i >= 5) {
                changes.addAll(iterator.removeWithoutReconnect());
            }
            i++;
        }
        applyChanges(changes);
        final OwlapiListIterator<NamedResource> it = iterator();
        int count = 0;
        while (it.hasNext()) {
            it.next();
            count++;
        }
        assertEquals(5, count);
    }

    @Test
    public void testReplaceHead() {
        final List<URI> lst = new ArrayList<>(ListTestHelper.LIST_ITEMS.subList(0, 5));
        testHelper.persistList(lst);
        lst.set(0, ListTestHelper.LIST_ITEMS.get(8));
        iterator.next();
        final List<TransactionalChange> changes = new ArrayList<>(iterator.replaceNode(NamedResource.create(lst.get(0))));
        applyChanges(changes);

        final OwlapiListIterator<NamedResource> it = iterator();
        verifyIterationContent(it, lst);
    }

    @Test
    public void testReplaceNodeInsideList() {
        final List<URI> lst = new ArrayList<>(ListTestHelper.LIST_ITEMS.subList(0, 5));
        testHelper.persistList(lst);
        int replaceIndex = 2;
        lst.set(replaceIndex, ListTestHelper.LIST_ITEMS.get(8));
        int i = 0;
        while (iterator.hasNext() && i < replaceIndex) {
            iterator.next();
            i++;
        }
        iterator.next();
        final List<TransactionalChange> changes =
                new ArrayList<>(iterator.replaceNode(NamedResource.create(lst.get(replaceIndex))));
        applyChanges(changes);

        final OwlapiListIterator<NamedResource> it = iterator();
        verifyIterationContent(it, lst);
    }

    @Test
    public void testReplaceLastNode() {
        final List<URI> lst = new ArrayList<>(ListTestHelper.LIST_ITEMS.subList(0, 5));
        testHelper.persistList(lst);
        int replaceIndex = lst.size() - 1;
        lst.set(replaceIndex, ListTestHelper.LIST_ITEMS.get(8));
        int i = 0;
        while (iterator.hasNext() && i < replaceIndex) {
            iterator.next();
            i++;
        }
        iterator.next();
        final List<TransactionalChange> changes =
                new ArrayList<>(iterator.replaceNode(NamedResource.create(lst.get(replaceIndex))));
        applyChanges(changes);

        final OwlapiListIterator<NamedResource> it = iterator();
        verifyIterationContent(it, lst);
    }
}
