/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.AxiomAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import org.junit.Test;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;

import static cz.cvut.kbss.ontodriver.owlapi.list.ListHandlerTestBase.LIST_ITEMS;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

abstract class OwlapiListIteratorBase {

    static final String SUBJECT = "http://krizik.felk.cvut.cz/jopa#Individual";

    OntologySnapshot snapshot;
    ListTestHelper testHelper;
    AxiomAdapter axiomAdapter;

    OwlapiListIterator iterator;

    public void setUp() throws Exception {
        this.snapshot = TestUtils.initRealOntology(null);
        this.axiomAdapter = new AxiomAdapter(snapshot.getDataFactory(), "en");
    }

    abstract OwlapiListIterator iterator();

    @Test(expected = NoSuchElementException.class)
    public void nextWithoutMoreElementsThrowsException() {
        testHelper.persistList(LIST_ITEMS);
        while (iterator.hasNext()) {
            iterator.next();
        }
        iterator.next();
    }

    @Test
    public void testBasicIteration() {
        testHelper.persistList(LIST_ITEMS);
        verifyIterationContent(iterator, LIST_ITEMS);
    }

    private void verifyIterationContent(OwlapiListIterator it, List<URI> expected) {
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
        testHelper.persistList(LIST_ITEMS);
        final List<OWLOntologyChange> changes = new ArrayList<>();
        while (iterator.hasNext()) {
            iterator.next();
            changes.addAll(iterator.removeWithoutReconnect());
        }
        snapshot.getOntologyManager().applyChanges(changes);
        final OwlapiListIterator it = iterator();
        assertFalse(it.hasNext());
    }

    @Test
    public void removeWithoutReconnectClearsRestOfList() {
        testHelper.persistList(LIST_ITEMS);
        final List<OWLOntologyChange> changes = new ArrayList<>();
        int i = 0;
        while (iterator.hasNext()) {
            iterator.next();
            if (i >= 5) {
                changes.addAll(iterator.removeWithoutReconnect());
            }
            i++;
        }
        snapshot.getOntologyManager().applyChanges(changes);
        final OwlapiListIterator it = iterator();
        int count = 0;
        while (it.hasNext()) {
            it.next();
            count++;
        }
        assertEquals(5, count);
    }

    @Test
    public void testReplaceHead() {
        final List<URI> lst = new ArrayList<>(LIST_ITEMS.subList(0, 5));
        testHelper.persistList(lst);
        lst.set(0, LIST_ITEMS.get(8));
        iterator.next();
        final List<OWLOntologyChange> changes = new ArrayList<>(iterator.replaceNode(NamedResource.create(lst.get(0))));
        snapshot.getOntologyManager().applyChanges(changes);

        final OwlapiListIterator it = iterator();
        verifyIterationContent(it, lst);
    }

    @Test
    public void testReplaceNodeInsideList() {
        final List<URI> lst = new ArrayList<>(LIST_ITEMS.subList(0, 5));
        testHelper.persistList(lst);
        int replaceIndex = 2;
        lst.set(replaceIndex, LIST_ITEMS.get(8));
        int i = 0;
        while (iterator.hasNext() && i < replaceIndex) {
            iterator.next();
            i++;
        }
        iterator.next();
        final List<OWLOntologyChange> changes =
                new ArrayList<>(iterator.replaceNode(NamedResource.create(lst.get(replaceIndex))));
        snapshot.getOntologyManager().applyChanges(changes);

        final OwlapiListIterator it = iterator();
        verifyIterationContent(it, lst);
    }

    @Test
    public void testReplaceLastNode() {
        final List<URI> lst = new ArrayList<>(LIST_ITEMS.subList(0, 5));
        testHelper.persistList(lst);
        int replaceIndex = lst.size() - 1;
        lst.set(replaceIndex, LIST_ITEMS.get(8));
        int i = 0;
        while (iterator.hasNext() && i < replaceIndex) {
            iterator.next();
            i++;
        }
        iterator.next();
        final List<OWLOntologyChange> changes =
                new ArrayList<>(iterator.replaceNode(NamedResource.create(lst.get(replaceIndex))));
        snapshot.getOntologyManager().applyChanges(changes);

        final OwlapiListIterator it = iterator();
        verifyIterationContent(it, lst);
    }
}
