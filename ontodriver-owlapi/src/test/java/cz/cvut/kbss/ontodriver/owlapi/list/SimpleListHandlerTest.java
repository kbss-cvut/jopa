package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;

import java.net.URI;
import java.util.Collection;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

public class SimpleListHandlerTest extends ListHandlerTestBase<SimpleListDescriptor, SimpleListValueDescriptor> {

    @Mock
    private OwlapiAdapter adapterMock;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        super.setUp();
        this.descriptor = new SimpleListDescriptorImpl(SUBJECT, HAS_LIST, HAS_NEXT);
        this.valueDescriptor = createDescriptor();
        // This snapshot contains the spied on objects
        final OntologySnapshot snapshotToUse = new OntologySnapshot(ontology, manager, dataFactory, reasonerMock);
        this.listHandler = new SimpleListHandler(adapterMock, snapshotToUse);
        this.testHelper = new SimpleListTestHelper(snapshotToUse, individual);
    }

    @Override
    SimpleListValueDescriptor createDescriptor() {
        return new SimpleListValueDescriptor(SUBJECT, HAS_LIST, HAS_NEXT);
    }

    @Test
    public void loadListReturnsEmptyListWhenNoListHeadIsFound() throws Exception {
        final Collection<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertTrue(result.isEmpty());
    }

    @Test
    public void loadsListWithHeadOnly() throws Exception {
        final List<URI> list = LIST_ITEMS.subList(0, 1);
        testHelper.persistList(list);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertEquals(list.size(), result.size());
        for (int i = 0; i < list.size(); i++) {
            assertEquals(HAS_LIST, result.get(i).getAssertion());
            assertEquals(list.get(i), result.get(i).getValue().getValue().getIdentifier());
        }
    }

    @Test
    public void loadsListWithMultipleItems() throws Exception {
        testHelper.persistList(LIST_ITEMS);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertEquals(LIST_ITEMS.size(), result.size());
        for (int i = 0; i < LIST_ITEMS.size(); i++) {
            if (i == 0) {
                assertEquals(HAS_LIST, result.get(i).getAssertion());
            } else {
                assertEquals(HAS_NEXT, result.get(i).getAssertion());
            }
            assertEquals(LIST_ITEMS.get(i), result.get(i).getValue().getValue().getIdentifier());
        }
    }

    @Test(expected = IntegrityConstraintViolatedException.class)
    public void loadingListWithItemWithMultipleSuccessorsThrowsException() throws Exception {
        testHelper.persistList(LIST_ITEMS.subList(0, 5));
        addExtraSuccessor(LIST_ITEMS.get(2), LIST_ITEMS.get(8));
        listHandler.loadList(descriptor);
    }

    private void addExtraSuccessor(URI target, URI successor) {
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(SequencesVocabulary.s_p_hasNext));
        manager.addAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(hasNext, dataFactory.getOWLNamedIndividual(
                        IRI.create(target)), dataFactory.getOWLNamedIndividual(IRI.create(successor))));
    }

    @Test
    public void loadsListWithInferredProperties() throws Exception {
        initReasoner(LIST_ITEMS);
        final SimpleListDescriptor infDescriptor = new SimpleListDescriptorImpl(SUBJECT,
                Assertion.createObjectPropertyAssertion(HAS_LIST.getIdentifier(), true),
                Assertion.createObjectPropertyAssertion(HAS_NEXT.getIdentifier(), true));
        final List<Axiom<NamedResource>> result = listHandler.loadList(infDescriptor);
        assertEquals(LIST_ITEMS.size(), result.size());
        for (int i = 0; i < LIST_ITEMS.size(); i++) {
            if (i == 0) {
                assertEquals(HAS_LIST, result.get(i).getAssertion());
            } else {
                assertEquals(HAS_NEXT, result.get(i).getAssertion());
            }
            assertEquals(LIST_ITEMS.get(i), result.get(i).getValue().getValue().getIdentifier());
        }
        verify(reasonerMock)
                .getObjectPropertyValues(dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier())),
                        dataFactory.getOWLObjectProperty(IRI.create(HAS_LIST.getIdentifier())));
        verify(reasonerMock, times(LIST_ITEMS.size()))
                .getObjectPropertyValues(any(OWLNamedIndividual.class),
                        eq(dataFactory.getOWLObjectProperty(IRI.create(HAS_NEXT.getIdentifier()))));
    }

    private void initReasoner(List<URI> items) {
        final OWLObjectProperty hasList = dataFactory.getOWLObjectProperty(IRI.create(HAS_LIST.getIdentifier()));
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(HAS_NEXT.getIdentifier()));
        when(reasonerMock
                .getObjectPropertyValues(dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier())),
                        hasList))
                .thenReturn(new OWLNamedIndividualNodeSet(dataFactory.getOWLNamedIndividual(IRI.create(items.get(0)))));
        for (int i = 1; i < items.size(); i++) {
            when(reasonerMock
                    .getObjectPropertyValues(dataFactory.getOWLNamedIndividual(IRI.create(items.get(i - 1))), hasNext))
                    .thenReturn(
                            new OWLNamedIndividualNodeSet(dataFactory.getOWLNamedIndividual(IRI.create(items.get(i)))));
        }
        when(reasonerMock
                .getObjectPropertyValues(dataFactory.getOWLNamedIndividual(IRI.create(items.get(items.size() - 1))),
                        hasNext)).thenReturn(new OWLNamedIndividualNodeSet());
    }

    @Test
    public void persistEmptyListDoesNothing() throws Exception {
        listHandler.persistList(valueDescriptor);
        verify(manager, never()).addAxiom(eq(ontology), any(OWLAxiom.class));
        verify(manager, never()).applyChanges(anyList());
    }

    @Test
    public void persistListWithOneElementCreatesOnlyHead() throws Exception {
        valueDescriptor.addValue(NamedResource.create(LIST_ITEMS.get(0)));
        listHandler.persistList(valueDescriptor);

        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(manager).applyChanges(captor.capture());
        final List<?> args = captor.getValue();
        assertEquals(1, args.size());
        final AddAxiom ax = (AddAxiom) args.get(0);
        final OWLAxiom axiom = ax.getAxiom();
        assertTrue(axiom instanceof OWLObjectPropertyAssertionAxiom);
        final OWLObjectProperty property = axiom.getObjectPropertiesInSignature().iterator().next();
        assertEquals(HAS_LIST.getIdentifier(), property.getIRI().toURI());
        final OWLIndividual value = ((OWLObjectPropertyAssertionAxiom) axiom).getObject();
        assertEquals(LIST_ITEMS.get(0), value.asOWLNamedIndividual().getIRI().toURI());
        verify(adapterMock).addTransactionalChanges(anyList());
    }

    @Test
    public void persistNonEmptyListCreatesListInOntology() throws Exception {
        LIST_ITEMS.forEach(item -> valueDescriptor.addValue(NamedResource.create(item)));
        listHandler.persistList(valueDescriptor);

        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(manager).applyChanges(captor.capture());
        final List<?> args = captor.getValue();
        assertEquals(LIST_ITEMS.size(), args.size());
        for (int i = 0; i < args.size(); i++) {
            final AddAxiom ax = (AddAxiom) args.get(i);
            final OWLAxiom axiom = ax.getAxiom();
            assertTrue(axiom instanceof OWLObjectPropertyAssertionAxiom);
            final OWLObjectPropertyAssertionAxiom assertionAxiom = (OWLObjectPropertyAssertionAxiom) axiom;
            if (i == 0) {
                assertEquals(SUBJECT.getIdentifier(),
                        assertionAxiom.getSubject().asOWLNamedIndividual().getIRI().toURI());
                assertEquals(HAS_LIST.getIdentifier(),
                        assertionAxiom.getProperty().asOWLObjectProperty().getIRI().toURI());
            } else {
                assertEquals(LIST_ITEMS.get(i - 1),
                        assertionAxiom.getSubject().asOWLNamedIndividual().getIRI().toURI());
                assertEquals(HAS_NEXT.getIdentifier(),
                        assertionAxiom.getProperty().asOWLObjectProperty().getIRI().toURI());
            }
            assertEquals(LIST_ITEMS.get(i), assertionAxiom.getObject().asOWLNamedIndividual().getIRI().toURI());
        }
        verify(adapterMock).addTransactionalChanges(anyList());
    }
}
