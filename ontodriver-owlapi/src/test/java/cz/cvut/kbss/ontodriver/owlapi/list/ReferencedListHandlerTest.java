/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;

import java.net.URI;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class ReferencedListHandlerTest
        extends ListHandlerTestBase<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    private OWLObjectProperty hasListProperty;
    private OWLObjectProperty hasNextProperty;
    private OWLObjectProperty hasContentProperty;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        super.setUp();
        this.descriptor = new ReferencedListDescriptorImpl(SUBJECT, HAS_LIST, HAS_NEXT, HAS_CONTENT);
        this.valueDescriptor = createDescriptor();
        final OntologySnapshot snapshotToUse = new OntologySnapshot(ontology, manager, dataFactory, reasonerMock);
        this.testHelper = new ReferencedListTestHelper(snapshotToUse, individual, SUBJECT.toString());
        this.listHandler = new ReferencedListHandler(adapterMock, snapshotToUse);
        this.hasListProperty = dataFactory.getOWLObjectProperty(IRI.create(HAS_LIST.getIdentifier()));
        this.hasNextProperty = dataFactory.getOWLObjectProperty(IRI.create(HAS_NEXT.getIdentifier()));
        this.hasContentProperty = dataFactory.getOWLObjectProperty(IRI.create(HAS_CONTENT.getIdentifier()));
    }

    @Override
    ReferencedListValueDescriptor createDescriptor() {
        return new ReferencedListValueDescriptor(SUBJECT, HAS_LIST, HAS_NEXT, HAS_CONTENT);
    }

    @Test
    public void loadListReturnsEmptyListWhenNoHeadIsFound() {
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    public void loadListLoadsSingleElementListWithHeadOnly() {
        final List<URI> headOnly = LIST_ITEMS.subList(0, 1);
        testHelper.persistList(headOnly);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertEquals(1, result.size());
        assertEquals(headOnly.get(0), result.get(0).getValue().getValue().getIdentifier());
    }

    @Test
    public void loadListWithMultipleItems() {
        testHelper.persistList(LIST_ITEMS);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertEquals(LIST_ITEMS.size(), result.size());
        for (int i = 0; i < LIST_ITEMS.size(); i++) {
            assertEquals(LIST_ITEMS.get(i), result.get(i).getValue().getValue().getIdentifier());
        }
    }

    @Test
    public void loadingListWithItemWithMultipleSuccessorsThrowsException() {
        testHelper.persistList(LIST_ITEMS.subList(0, 5));
        addExtraPropertyValue(ListTestHelper.HAS_NEXT_PROPERTY, LIST_ITEMS.get(8));
        assertThrows(IntegrityConstraintViolatedException.class, () -> listHandler.loadList(descriptor));
    }

    private void addExtraPropertyValue(String property, URI value) {
        final OWLNamedIndividual node = dataFactory
                .getOWLNamedIndividual(IRI.create(
                        SUBJECT.getIdentifier().toString() + ReferencedListTestHelper.SEQUENCE_NODE_SUFFIX + "2"));
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(property));
        manager.addAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(hasNext, node,
                        dataFactory.getOWLNamedIndividual(IRI.create(value))));
    }

    @Test
    public void loadingListWithNodeWithMultipleContentElementsThrowsException() {
        testHelper.persistList(LIST_ITEMS.subList(0, 8));
        addExtraPropertyValue(ListTestHelper.HAS_CONTENT_PROPERTY, LIST_ITEMS.get(0));
        assertThrows(IntegrityConstraintViolatedException.class, () -> listHandler.loadList(descriptor));
    }

    @Test
    public void loadListWithInferredNodeContent() {
        initReasoner();
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(SUBJECT,
                HAS_LIST, HAS_NEXT, Assertion.createObjectPropertyAssertion(HAS_CONTENT.getIdentifier(), true));
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertEquals(1, result.size());
        assertEquals(LIST_ITEMS.get(0), result.get(0).getValue().getValue().getIdentifier());
    }

    private void initReasoner() {
        final OWLNamedIndividual node = dataFactory.getOWLNamedIndividual(
                IRI.create(SUBJECT + ReferencedListTestHelper.SEQUENCE_NODE_SUFFIX + "0"));
        final OWLNamedIndividual owner = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
        final NodeSet<OWLNamedIndividual> nodeSet = new OWLNamedIndividualNodeSet(node);
        when(reasonerMock.getObjectPropertyValues(owner, hasListProperty)).thenReturn(nodeSet);
        final OWLNamedIndividual content = dataFactory.getOWLNamedIndividual(IRI.create(LIST_ITEMS.get(0)));
        final NodeSet<OWLNamedIndividual> valueSet = new OWLNamedIndividualNodeSet(content);
        when(reasonerMock.getObjectPropertyValues(node, hasContentProperty)).thenReturn(valueSet);
        when(reasonerMock.getObjectPropertyValues(node, hasNextProperty)).thenReturn(new OWLNamedIndividualNodeSet());
    }

    @Test
    public void loadListWithInferredHead() {
        initReasoner();
        final ReferencedListDescriptor descriptor = new ReferencedListDescriptorImpl(SUBJECT,
                Assertion.createObjectPropertyAssertion(HAS_LIST.getIdentifier(), true), HAS_NEXT, HAS_CONTENT);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertEquals(1, result.size());
        assertEquals(LIST_ITEMS.get(0), result.get(0).getValue().getValue().getIdentifier());
    }

    @Test
    public void persistEmptyListDoesNothing() {
        listHandler.persistList(valueDescriptor);
        verify(manager, never()).applyChanges(anyList());
    }

    @Test
    public void persistListWithHeadOnly() {
        valueDescriptor.addValue(NamedResource.create(LIST_ITEMS.get(0)));
        listHandler.persistList(valueDescriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(manager).applyChanges(captor.capture());
        final List<?> changes = captor.getValue();
        verifyAddedChanges(changes, LIST_ITEMS.subList(0, 1));
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
        LIST_ITEMS.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));
        listHandler.persistList(valueDescriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(manager).applyChanges(captor.capture());
        final List<?> changes = captor.getValue();
        verifyAddedChanges(changes, LIST_ITEMS);
        verify(adapterMock).addTransactionalChanges(anyList());
    }
}
