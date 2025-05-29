/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class SimpleListHandlerTest {

    OWLOntology ontology;
    OWLOntologyManager manager;
    OWLDataFactory dataFactory;
    OWLNamedIndividual individual;

    @Mock
    OWLReasoner reasonerMock;

    @Mock
    OwlapiAdapter adapterMock;

    SimpleListDescriptor descriptor;
    SimpleListValueDescriptor valueDescriptor;
    ListTestHelper testHelper;

    SimpleListHandler listHandler;

    @BeforeEach
    public void setUp() throws Exception {
        final OntologySnapshot realSnapshot = TestUtils.initRealOntology(reasonerMock);
        this.ontology = spy(realSnapshot.ontology());
        this.manager = spy(realSnapshot.ontologyManager());
        this.dataFactory = realSnapshot.dataFactory();
        this.individual = dataFactory.getOWLNamedIndividual(IRI.create(ListTestHelper.SUBJECT.getIdentifier()));
        this.descriptor = new SimpleListDescriptorImpl(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT);
        this.valueDescriptor = createDescriptor();
        // This snapshot contains the spied on objects
        final OntologySnapshot snapshotToUse = new OntologySnapshot(ontology, manager, dataFactory, reasonerMock);
        this.listHandler = new SimpleListHandler(adapterMock, snapshotToUse);
        this.testHelper = new SimpleListTestHelper(snapshotToUse, individual);
    }

    SimpleListValueDescriptor createDescriptor() {
        return new SimpleListValueDescriptor(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT);
    }

    @Test
    public void loadListReturnsEmptyListWhenNoListHeadIsFound() {
        final Collection<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertTrue(result.isEmpty());
    }

    @Test
    public void loadsListWithHeadOnly() {
        final List<URI> list = ListTestHelper.LIST_ITEMS.subList(0, 1);
        testHelper.persistList(list);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertEquals(list.size(), result.size());
        for (int i = 0; i < list.size(); i++) {
            assertEquals(ListTestHelper.HAS_LIST, result.get(i).getAssertion());
            assertEquals(list.get(i), result.get(i).getValue().getValue().getIdentifier());
        }
    }

    @Test
    public void loadsListWithMultipleItems() {
        testHelper.persistList(ListTestHelper.LIST_ITEMS);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertEquals(ListTestHelper.LIST_ITEMS.size(), result.size());
        verifyLoadedList(result);
    }

    private void verifyLoadedList(List<Axiom<NamedResource>> result) {
        for (int i = 0; i < ListTestHelper.LIST_ITEMS.size(); i++) {
            if (i == 0) {
                assertEquals(ListTestHelper.HAS_LIST, result.get(i).getAssertion());
            } else {
                assertEquals(ListTestHelper.HAS_NEXT, result.get(i).getAssertion());
            }
            assertEquals(ListTestHelper.LIST_ITEMS.get(i), result.get(i).getValue().getValue().getIdentifier());
        }
    }

    @Test
    public void loadingListWithItemWithMultipleSuccessorsThrowsException() {
        testHelper.persistList(ListTestHelper.LIST_ITEMS.subList(0, 5));
        addExtraSuccessor(ListTestHelper.LIST_ITEMS.get(2), ListTestHelper.LIST_ITEMS.get(8));
        assertThrows(IntegrityConstraintViolatedException.class, () -> listHandler.loadList(descriptor));
    }

    private void addExtraSuccessor(URI target, URI successor) {
        final OWLObjectProperty hasNext = dataFactory
                .getOWLObjectProperty(IRI.create(ListTestHelper.HAS_NEXT_PROPERTY));
        manager.addAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(hasNext, dataFactory.getOWLNamedIndividual(
                        IRI.create(target)), dataFactory.getOWLNamedIndividual(IRI.create(successor))));
    }

    @Test
    public void loadsListWithInferredProperties() {
        initReasoner();
        final SimpleListDescriptor infDescriptor = new SimpleListDescriptorImpl(ListTestHelper.SUBJECT,
                Assertion.createObjectPropertyAssertion(ListTestHelper.HAS_LIST.getIdentifier(), true),
                Assertion.createObjectPropertyAssertion(ListTestHelper.HAS_NEXT.getIdentifier(), true));
        final List<Axiom<NamedResource>> result = listHandler.loadList(infDescriptor);
        assertEquals(ListTestHelper.LIST_ITEMS.size(), result.size());
        verifyLoadedList(result);
        verify(reasonerMock)
                .getObjectPropertyValues(dataFactory.getOWLNamedIndividual(IRI.create(ListTestHelper.SUBJECT.getIdentifier())),
                        dataFactory.getOWLObjectProperty(IRI.create(ListTestHelper.HAS_LIST.getIdentifier())));
        verify(reasonerMock, times(ListTestHelper.LIST_ITEMS.size()))
                .getObjectPropertyValues(any(OWLNamedIndividual.class),
                        eq(dataFactory.getOWLObjectProperty(IRI.create(ListTestHelper.HAS_NEXT.getIdentifier()))));
    }

    private void initReasoner() {
        final OWLObjectProperty hasList = dataFactory.getOWLObjectProperty(IRI.create(ListTestHelper.HAS_LIST.getIdentifier()));
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(ListTestHelper.HAS_NEXT.getIdentifier()));
        when(reasonerMock
                .getObjectPropertyValues(dataFactory.getOWLNamedIndividual(IRI.create(ListTestHelper.SUBJECT.getIdentifier())),
                        hasList))
                .thenReturn(new OWLNamedIndividualNodeSet(dataFactory.getOWLNamedIndividual(IRI.create(ListTestHelper.LIST_ITEMS.get(0)))));
        for (int i = 1; i < ListTestHelper.LIST_ITEMS.size(); i++) {
            when(reasonerMock
                    .getObjectPropertyValues(dataFactory.getOWLNamedIndividual(IRI.create(ListTestHelper.LIST_ITEMS.get(i - 1))), hasNext))
                    .thenReturn(
                            new OWLNamedIndividualNodeSet(dataFactory.getOWLNamedIndividual(IRI.create(ListTestHelper.LIST_ITEMS.get(i)))));
        }
        when(reasonerMock
                .getObjectPropertyValues(dataFactory.getOWLNamedIndividual(IRI.create(ListTestHelper.LIST_ITEMS.get(ListTestHelper.LIST_ITEMS.size() - 1))),
                        hasNext)).thenReturn(new OWLNamedIndividualNodeSet());
    }

    @Test
    public void persistEmptyListDoesNothing() {
        listHandler.persistList(valueDescriptor);
        verify(manager, never()).addAxiom(eq(ontology), any(OWLAxiom.class));
        verify(manager, never()).applyChanges(anyList());
    }

    @Test
    public void persistListWithOneElementCreatesOnlyHead() {
        valueDescriptor.addValue(NamedResource.create(ListTestHelper.LIST_ITEMS.get(0)));
        listHandler.persistList(valueDescriptor);

        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(manager).applyChanges(captor.capture());
        final List<?> args = captor.getValue();
        assertEquals(1, args.size());
        final AddAxiom ax = (AddAxiom) args.get(0);
        final OWLAxiom axiom = ax.getAxiom();
        assertInstanceOf(OWLObjectPropertyAssertionAxiom.class, axiom);
        final OWLObjectProperty property = axiom.objectPropertiesInSignature().iterator().next();
        assertEquals(ListTestHelper.HAS_LIST.getIdentifier(), property.getIRI().toURI());
        final OWLIndividual value = ((OWLObjectPropertyAssertionAxiom) axiom).getObject();
        assertEquals(ListTestHelper.LIST_ITEMS.get(0), value.asOWLNamedIndividual().getIRI().toURI());
        verify(adapterMock).addTransactionalChanges(anyList());
    }

    @Test
    public void persistNonEmptyListCreatesListInOntology() {
        ListTestHelper.LIST_ITEMS.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));
        listHandler.persistList(valueDescriptor);

        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(manager).applyChanges(captor.capture());
        final List<?> args = captor.getValue();
        assertEquals(ListTestHelper.LIST_ITEMS.size(), args.size());
        for (int i = 0; i < args.size(); i++) {
            final AddAxiom ax = (AddAxiom) args.get(i);
            final OWLAxiom axiom = ax.getAxiom();
            assertInstanceOf(OWLObjectPropertyAssertionAxiom.class, axiom);
            final OWLObjectPropertyAssertionAxiom assertionAxiom = (OWLObjectPropertyAssertionAxiom) axiom;
            if (i == 0) {
                assertEquals(ListTestHelper.SUBJECT.getIdentifier(),
                        assertionAxiom.getSubject().asOWLNamedIndividual().getIRI().toURI());
                assertEquals(ListTestHelper.HAS_LIST.getIdentifier(),
                        assertionAxiom.getProperty().asOWLObjectProperty().getIRI().toURI());
            } else {
                assertEquals(ListTestHelper.LIST_ITEMS.get(i - 1),
                        assertionAxiom.getSubject().asOWLNamedIndividual().getIRI().toURI());
                assertEquals(ListTestHelper.HAS_NEXT.getIdentifier(),
                        assertionAxiom.getProperty().asOWLObjectProperty().getIRI().toURI());
            }
            assertEquals(ListTestHelper.LIST_ITEMS.get(i), assertionAxiom.getObject().asOWLNamedIndividual().getIRI()
                                                                         .toURI());
        }
        verify(adapterMock).addTransactionalChanges(anyList());
    }

    @Test
    public void updateListToEmptyClearsList() {
        final List<URI> origList = ListTestHelper.LIST_ITEMS.subList(0, 5);
        origList.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));
        listHandler.persistList(valueDescriptor);

        final SimpleListValueDescriptor updatedDescriptor = createDescriptor();
        listHandler.updateList(updatedDescriptor);

        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertTrue(result.isEmpty());
    }

    @Test
    public void updateEmptyListWithNonEmptyPersistsNewOne() {
        final List<URI> updated = ListTestHelper.LIST_ITEMS.subList(0, 5);
        updated.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        listHandler.updateList(valueDescriptor);

        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        ListTestHelper.verifyListContent(updated, result);
    }

    @Test
    public void updateListByRemovingElementsFromTheEnd() {
        testHelper.persistList(ListTestHelper.LIST_ITEMS);
        final List<URI> subList = ListTestHelper.LIST_ITEMS.subList(0, 5);
        subList.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        listHandler.updateList(valueDescriptor);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        ListTestHelper.verifyListContent(subList, result);
    }

    @Test
    public void updateListByReplacingSomeElementsButKeepingSize() {
        final List<URI> origList = ListTestHelper.LIST_ITEMS.subList(0, 6);
        testHelper.persistList(origList);

        final List<URI> updated = new ArrayList<>(origList);
        updated.set(2, ListTestHelper.LIST_ITEMS.get(7));
        updated.set(4, ListTestHelper.LIST_ITEMS.get(9));
        updated.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        listHandler.updateList(valueDescriptor);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        ListTestHelper.verifyListContent(updated, result);
    }

    @Test
    public void updateListByReplacingSomeElementsAndAddingNewOnes() {
        final List<URI> origList = ListTestHelper.LIST_ITEMS.subList(0, 6);
        testHelper.persistList(origList);
        final List<URI> updated = new ArrayList<>(origList);
        updated.set(2, ListTestHelper.LIST_ITEMS.get(7));
        updated.set(3, ListTestHelper.LIST_ITEMS.get(8));
        updated.add(ListTestHelper.LIST_ITEMS.get(9));
        updated.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        listHandler.updateList(valueDescriptor);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        ListTestHelper.verifyListContent(updated, result);
    }

    @Test
    public void updateListByReplacingSomeElementsAndRemovingSome() {
        final List<URI> origList = ListTestHelper.LIST_ITEMS.subList(0, 6);
        testHelper.persistList(origList);
        final List<URI> updated = new ArrayList<>(origList);
        updated.set(0, ListTestHelper.LIST_ITEMS.get(7));
        updated.set(1, ListTestHelper.LIST_ITEMS.get(8));
        updated.remove(updated.size() - 1);
        updated.remove(updated.size() - 2);
        updated.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        listHandler.updateList(valueDescriptor);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        ListTestHelper.verifyListContent(updated, result);
    }
}
