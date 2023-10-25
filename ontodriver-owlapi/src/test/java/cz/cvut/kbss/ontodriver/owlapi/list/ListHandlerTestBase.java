/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.spy;

public abstract class ListHandlerTestBase<D extends ListDescriptor, V extends ListValueDescriptor> {

    static final List<URI> LIST_ITEMS = initListItems();
    static final NamedResource SUBJECT = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa#Owner");
    static final Assertion HAS_LIST = Assertion
            .createObjectPropertyAssertion(URI.create(ListTestHelper.HAS_LIST_PROPERTY), false);
    static final Assertion HAS_NEXT = Assertion
            .createObjectPropertyAssertion(URI.create(ListTestHelper.HAS_NEXT_PROPERTY), false);
    static final Assertion HAS_CONTENT = Assertion
            .createObjectPropertyAssertion(URI.create(ListTestHelper.HAS_CONTENT_PROPERTY), false);

    OWLOntology ontology;
    OWLOntologyManager manager;
    OWLDataFactory dataFactory;
    OWLNamedIndividual individual;

    @Mock
    OWLReasoner reasonerMock;

    @Mock
    OwlapiAdapter adapterMock;

    D descriptor;
    V valueDescriptor;
    ListTestHelper testHelper;
    ListHandler<D, V> listHandler;

    private static List<URI> initListItems() {
        final List<URI> lst = new ArrayList<>(10);
        for (int i = 0; i < 10; i++) {
            lst.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#ListItem_" + i));
        }
        return lst;
    }

    public void setUp() throws Exception {
        final OntologySnapshot realSnapshot = TestUtils.initRealOntology(reasonerMock);
        this.ontology = spy(realSnapshot.getOntology());
        this.manager = spy(realSnapshot.getOntologyManager());
        this.dataFactory = realSnapshot.getDataFactory();
        this.individual = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
    }

    abstract V createDescriptor();

    @Test
    public void updateListToEmptyClearsList() {
        final List<URI> origList = LIST_ITEMS.subList(0, 5);
        origList.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));
        listHandler.persistList(valueDescriptor);

        final V updatedDescriptor = createDescriptor();
        listHandler.updateList(updatedDescriptor);

        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertTrue(result.isEmpty());
    }

    @Test
    public void updateEmptyListWithNonEmptyPersistsNewOne() {
        final List<URI> updated = LIST_ITEMS.subList(0, 5);
        updated.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        listHandler.updateList(valueDescriptor);

        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        verifyUpdatedListContent(updated, result);
    }

    private void verifyUpdatedListContent(List<URI> expected, List<Axiom<NamedResource>> actual) {
        assertEquals(expected.size(), actual.size());
        for (int i = 0; i < expected.size(); i++) {
            assertEquals(expected.get(i), actual.get(i).getValue().getValue().getIdentifier());
        }
    }

    @Test
    public void updateListByRemovingElementsFromTheEnd() {
        testHelper.persistList(LIST_ITEMS);
        final List<URI> subList = LIST_ITEMS.subList(0, 5);
        subList.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        listHandler.updateList(valueDescriptor);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        verifyUpdatedListContent(subList, result);
    }

    @Test
    public void updateListByReplacingSomeElementsButKeepingSize() {
        final List<URI> origList = LIST_ITEMS.subList(0, 6);
        testHelper.persistList(origList);

        final List<URI> updated = new ArrayList<>(origList);
        updated.set(2, LIST_ITEMS.get(7));
        updated.set(4, LIST_ITEMS.get(9));
        updated.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        listHandler.updateList(valueDescriptor);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        verifyUpdatedListContent(updated, result);
    }

    @Test
    public void updateListByReplacingSomeElementsAndAddingNewOnes() {
        final List<URI> origList = LIST_ITEMS.subList(0, 6);
        testHelper.persistList(origList);
        final List<URI> updated = new ArrayList<>(origList);
        updated.set(2, LIST_ITEMS.get(7));
        updated.set(3, LIST_ITEMS.get(8));
        updated.add(LIST_ITEMS.get(9));
        updated.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        listHandler.updateList(valueDescriptor);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        verifyUpdatedListContent(updated, result);
    }

    @Test
    public void updateListByReplacingSomeElementsAndRemovingSome() {
        final List<URI> origList = LIST_ITEMS.subList(0, 6);
        testHelper.persistList(origList);
        final List<URI> updated = new ArrayList<>(origList);
        updated.set(0, LIST_ITEMS.get(7));
        updated.set(1, LIST_ITEMS.get(8));
        updated.remove(updated.size() - 1);
        updated.remove(updated.size() - 2);
        updated.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        listHandler.updateList(valueDescriptor);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        verifyUpdatedListContent(updated, result);
    }
}
