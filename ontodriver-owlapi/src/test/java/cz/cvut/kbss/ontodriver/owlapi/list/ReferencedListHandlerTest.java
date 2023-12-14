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
package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.MultilingualString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.anyList;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class ReferencedListHandlerTest {

    OWLOntology ontology;
    OWLOntologyManager manager;
    OWLDataFactory dataFactory;
    OWLNamedIndividual individual;

    @Mock
    OWLReasoner reasonerMock;

    @Mock
    OwlapiAdapter adapterMock;

    ReferencedListDescriptor descriptor;
    ReferencedListValueDescriptor<NamedResource> valueDescriptor;
    ListTestHelper testHelper;
    ReferencedListHandler sut;

    private OWLObjectProperty hasListProperty;
    private OWLObjectProperty hasNextProperty;
    private OWLObjectProperty hasContentProperty;

    @BeforeEach
    public void setUp() throws Exception {
        final OntologySnapshot realSnapshot = TestUtils.initRealOntology(reasonerMock);
        this.ontology = spy(realSnapshot.getOntology());
        this.manager = spy(realSnapshot.getOntologyManager());
        this.dataFactory = realSnapshot.getDataFactory();
        this.individual = dataFactory.getOWLNamedIndividual(IRI.create(ListTestHelper.SUBJECT.getIdentifier()));
        this.descriptor = new ReferencedListDescriptorImpl(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT, ListTestHelper.HAS_CONTENT);
        this.valueDescriptor = createDescriptor();
        final OntologySnapshot snapshotToUse = new OntologySnapshot(ontology, manager, dataFactory, reasonerMock);
        this.testHelper = new ReferencedListTestHelper(snapshotToUse, individual, ListTestHelper.SUBJECT.toString());
        this.sut = new ReferencedListHandler(adapterMock, snapshotToUse);
        this.hasListProperty = dataFactory.getOWLObjectProperty(IRI.create(ListTestHelper.HAS_LIST.getIdentifier()));
        this.hasNextProperty = dataFactory.getOWLObjectProperty(IRI.create(ListTestHelper.HAS_NEXT.getIdentifier()));
        this.hasContentProperty = dataFactory.getOWLObjectProperty(IRI.create(ListTestHelper.HAS_CONTENT.getIdentifier()));
    }

    ReferencedListValueDescriptor<NamedResource> createDescriptor() {
        return new ReferencedListValueDescriptor<>(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT, ListTestHelper.HAS_CONTENT);
    }

    @Test
    public void loadListReturnsEmptyListWhenNoHeadIsFound() {
        final List<Axiom<?>> result = sut.loadList(descriptor);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    public void loadListLoadsSingleElementListWithHeadOnly() {
        final List<URI> headOnly = ListTestHelper.LIST_ITEMS.subList(0, 1);
        testHelper.persistList(headOnly);
        final List<Axiom<?>> result = sut.loadList(descriptor);
        assertEquals(1, result.size());
        final Object value = result.get(0).getValue().getValue();
        assertInstanceOf(NamedResource.class, value);
        assertEquals(headOnly.get(0), ((NamedResource) value).getIdentifier());
    }

    @Test
    public void loadListWithMultipleItems() {
        testHelper.persistList(ListTestHelper.LIST_ITEMS);
        final List<Axiom<?>> result = sut.loadList(descriptor);
        assertEquals(ListTestHelper.LIST_ITEMS.size(), result.size());
        for (int i = 0; i < ListTestHelper.LIST_ITEMS.size(); i++) {
            final Object value = result.get(i).getValue().getValue();
            assertInstanceOf(NamedResource.class, value);
            assertEquals(ListTestHelper.LIST_ITEMS.get(i), ((NamedResource) value).getIdentifier());
        }
    }

    @Test
    public void loadingListWithItemWithMultipleSuccessorsThrowsException() {
        testHelper.persistList(ListTestHelper.LIST_ITEMS.subList(0, 5));
        addExtraPropertyValue(ListTestHelper.HAS_NEXT_PROPERTY, ListTestHelper.LIST_ITEMS.get(8));
        assertThrows(IntegrityConstraintViolatedException.class, () -> sut.loadList(descriptor));
    }

    private void addExtraPropertyValue(String property, URI value) {
        final OWLNamedIndividual node = dataFactory
                .getOWLNamedIndividual(IRI.create(
                        ListTestHelper.SUBJECT.getIdentifier()
                                              .toString() + ReferencedListTestHelper.SEQUENCE_NODE_SUFFIX + "2"));
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(property));
        manager.addAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(hasNext, node,
                        dataFactory.getOWLNamedIndividual(IRI.create(value))));
    }

    @Test
    public void loadingListWithNodeWithMultipleContentElementsThrowsException() {
        testHelper.persistList(ListTestHelper.LIST_ITEMS.subList(0, 8));
        addExtraPropertyValue(ListTestHelper.HAS_CONTENT_PROPERTY, ListTestHelper.LIST_ITEMS.get(0));
        assertThrows(IntegrityConstraintViolatedException.class, () -> sut.loadList(descriptor));
    }

    @Test
    public void loadListWithInferredNodeContent() {
        initReasoner();
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(ListTestHelper.SUBJECT,
                ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT, Assertion.createObjectPropertyAssertion(ListTestHelper.HAS_CONTENT.getIdentifier(), true));
        final List<Axiom<?>> result = sut.loadList(descriptor);
        assertEquals(1, result.size());
        final Object value = result.get(0).getValue().getValue();
        assertInstanceOf(NamedResource.class, value);
        assertEquals(ListTestHelper.LIST_ITEMS.get(0), ((NamedResource) value).getIdentifier());
    }

    private void initReasoner() {
        final OWLNamedIndividual node = dataFactory.getOWLNamedIndividual(
                IRI.create(ListTestHelper.SUBJECT + ReferencedListTestHelper.SEQUENCE_NODE_SUFFIX + "0"));
        final OWLNamedIndividual owner = dataFactory.getOWLNamedIndividual(IRI.create(ListTestHelper.SUBJECT.getIdentifier()));
        final NodeSet<OWLNamedIndividual> nodeSet = new OWLNamedIndividualNodeSet(node);
        when(reasonerMock.getObjectPropertyValues(owner, hasListProperty)).thenReturn(nodeSet);
        final OWLNamedIndividual content = dataFactory.getOWLNamedIndividual(IRI.create(ListTestHelper.LIST_ITEMS.get(0)));
        final NodeSet<OWLNamedIndividual> valueSet = new OWLNamedIndividualNodeSet(content);
        when(reasonerMock.getObjectPropertyValues(node, hasContentProperty)).thenReturn(valueSet);
        when(reasonerMock.getObjectPropertyValues(node, hasNextProperty)).thenReturn(new OWLNamedIndividualNodeSet());
    }

    @Test
    public void loadListWithInferredHead() {
        initReasoner();
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(ListTestHelper.SUBJECT,
                Assertion.createObjectPropertyAssertion(ListTestHelper.HAS_LIST.getIdentifier(), true), ListTestHelper.HAS_NEXT, ListTestHelper.HAS_CONTENT);
        final List<Axiom<?>> result = sut.loadList(descriptor);
        assertEquals(1, result.size());
        final Object value = result.get(0).getValue().getValue();
        assertInstanceOf(NamedResource.class, value);
        assertEquals(ListTestHelper.LIST_ITEMS.get(0), ((NamedResource) value).getIdentifier());
    }

    @Test
    public void persistEmptyListDoesNothing() {
        sut.persistList(valueDescriptor);
        verify(manager, never()).applyChanges(anyList());
    }

    @Test
    public void persistListWithHeadOnly() {
        valueDescriptor.addValue(NamedResource.create(ListTestHelper.LIST_ITEMS.get(0)));
        sut.persistList(valueDescriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(manager).applyChanges(captor.capture());
        final List<?> changes = captor.getValue();
        verifyAddedChanges(changes, ListTestHelper.LIST_ITEMS.subList(0, 1));
        verify(adapterMock).addTransactionalChanges(anyList());
    }

    private void verifyAddedChanges(List<?> changes, List<URI> expectedElements) {
        assertEquals(expectedElements.size() * 2, changes.size());
        int i = 0;
        OWLNamedIndividual node = null;
        for (Object change : changes) {
            final AddAxiom ax = (AddAxiom) change;
            final OWLAxiom axiom = ax.getAxiom();
            assertTrue(axiom instanceof OWLObjectPropertyAssertionAxiom);
            final OWLObjectPropertyAssertionAxiom assertion = (OWLObjectPropertyAssertionAxiom) axiom;
            OWLObjectProperty property = axiom.objectPropertiesInSignature().iterator().next();
            if (property.equals(hasContentProperty)) {
                final OWLIndividual value = assertion.getObject();
                assertEquals(expectedElements.get(i / 2), value.asOWLNamedIndividual().getIRI().toURI());
                assertEquals(node, assertion.getSubject().asOWLNamedIndividual());
            } else {
                node = assertion.getObject().asOWLNamedIndividual();
                if (i == 0) {
                    assertEquals(hasListProperty, property);
                } else {
                    assertEquals(hasNextProperty, property);
                }
            }
            i++;
        }
    }

    @Test
    public void persistListWithMultipleElementsSavesTheList() {
        ListTestHelper.LIST_ITEMS.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));
        sut.persistList(valueDescriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(manager).applyChanges(captor.capture());
        final List<?> changes = captor.getValue();
        verifyAddedChanges(changes, ListTestHelper.LIST_ITEMS);
        verify(adapterMock).addTransactionalChanges(anyList());
    }

    @Test
    public void updateListToEmptyClearsList() {
        final List<URI> origList = ListTestHelper.LIST_ITEMS.subList(0, 5);
        origList.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));
        sut.persistList(valueDescriptor);

        final ReferencedListValueDescriptor<NamedResource> updatedDescriptor = createDescriptor();
        sut.updateList(updatedDescriptor);

        final List<Axiom<?>> result = sut.loadList(descriptor);
        assertTrue(result.isEmpty());
    }

    @Test
    public void updateEmptyListWithNonEmptyPersistsNewOne() {
        final List<URI> updated = ListTestHelper.LIST_ITEMS.subList(0, 5);
        updated.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        sut.updateList(valueDescriptor);

        final List<Axiom<?>> result = sut.loadList(descriptor);
        ListTestHelper.verifyListContent(updated, result);
    }

    @Test
    public void updateListByRemovingElementsFromTheEnd() {
        testHelper.persistList(ListTestHelper.LIST_ITEMS);
        final List<URI> subList = ListTestHelper.LIST_ITEMS.subList(0, 5);
        subList.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));

        sut.updateList(valueDescriptor);
        final List<Axiom<?>> result = sut.loadList(descriptor);
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

        sut.updateList(valueDescriptor);
        final List<Axiom<?>> result = sut.loadList(descriptor);
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

        sut.updateList(valueDescriptor);
        final List<Axiom<?>> result = sut.loadList(descriptor);
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

        sut.updateList(valueDescriptor);
        final List<Axiom<?>> result = sut.loadList(descriptor);
        ListTestHelper.verifyListContent(updated, result);
    }

    @Test
    void persistListSupportsPersistingListWithDataPropertyContent() {
        final List<Integer> values = IntStream.range(0, 5).boxed().collect(Collectors.toList());
        final ReferencedListValueDescriptor<Integer> valueDescriptor = new ReferencedListValueDescriptor<>(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT, Assertion.createDataPropertyAssertion(URI.create(ListTestHelper.HAS_CONTENT_PROPERTY), false));
        values.forEach(valueDescriptor::addValue);

        sut.persistList(valueDescriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(manager).applyChanges(captor.capture());
        final List<AddAxiom> changes = captor.getValue();
        assertEquals(values.size() * 2, changes.size());
        for (AddAxiom change : changes) {
            final OWLAxiom ax = change.getAxiom();
            if (ax.isOfType(AxiomType.DATA_PROPERTY_ASSERTION)) {
                final OWLDataPropertyAssertionAxiom dpaAx = (OWLDataPropertyAssertionAxiom) ax;
                assertEquals(ListTestHelper.HAS_CONTENT_PROPERTY, dpaAx.getProperty().asOWLDataProperty().getIRI()
                                                                       .toString());
                assertThat(values, hasItem((Integer) OwlapiUtils.owlLiteralToValue(dpaAx.getObject())));
            }
        }
    }

    @Test
    public void loadListLoadsListWithDataPropertyValues() {
        final List<Integer> values = IntStream.range(0, 10).boxed().collect(Collectors.toList());
        testHelper.persistList(values);
        final ReferencedListDescriptor desc = new ReferencedListDescriptorImpl(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST,
                ListTestHelper.HAS_NEXT,
                Assertion.createDataPropertyAssertion(URI.create(ListTestHelper.HAS_CONTENT_PROPERTY), false));
        final List<Axiom<?>> result = sut.loadList(desc);
        assertEquals(values.size(), result.size());
        for (int i = 0; i < values.size(); i++) {
            final Object value = result.get(i).getValue().getValue();
            assertInstanceOf(Integer.class, value);
            assertEquals(values.get(i), value);
        }
    }

    @Test
    void persistListSavesMultilingualStringTranslationsAsContentOfSingleNode() {
        final List<MultilingualString> refList = List.of(
                new MultilingualString(Map.of("en", "one", "cs", "jedna")),
                new MultilingualString(Map.of("en", "two", "cs", "dva"))
        );
        final ReferencedListValueDescriptor<MultilingualString> valueDescriptor = new ReferencedListValueDescriptor<>(ListTestHelper.SUBJECT, ListTestHelper.HAS_LIST, ListTestHelper.HAS_NEXT, Assertion.createDataPropertyAssertion(URI.create(ListTestHelper.HAS_CONTENT_PROPERTY), false));
        refList.forEach(valueDescriptor::addValue);

        sut.persistList(valueDescriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(manager).applyChanges(captor.capture());
        final List<AddAxiom> changes = captor.getValue();
        assertEquals(refList.size() * 3, changes.size());
        for (AddAxiom change : changes) {
            final OWLAxiom ax = change.getAxiom();
            if (ax.isOfType(AxiomType.DATA_PROPERTY_ASSERTION)) {
                final OWLDataPropertyAssertionAxiom dpaAx = (OWLDataPropertyAssertionAxiom) ax;
                assertEquals(ListTestHelper.HAS_CONTENT_PROPERTY, dpaAx.getProperty().asOWLDataProperty().getIRI()
                                                                       .toString());
                final LangString literalValue = (LangString) OwlapiUtils.owlLiteralToValue(dpaAx.getObject());
                assertTrue(literalValue.getLanguage().isPresent());
                final String lang = literalValue.getLanguage().get();
                final String value = literalValue.getValue();
                assertTrue(refList.stream().anyMatch(mls -> mls.getValue().containsKey(lang) && mls.getValue().get(lang)
                                                                                                   .equals(value)));
            }
        }
    }
}
