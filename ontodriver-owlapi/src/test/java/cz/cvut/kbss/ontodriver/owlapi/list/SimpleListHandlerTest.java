package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

public class SimpleListHandlerTest {

    private static final List<URI> LIST_ITEMS = initListItems();
    private static final NamedResource SUBJECT = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa#Owner");
    private static final Assertion HAS_LIST = Assertion
            .createObjectPropertyAssertion(URI.create(SequencesVocabulary.s_p_hasListProperty), false);
    private static final Assertion HAS_NEXT = Assertion
            .createObjectPropertyAssertion(URI.create(SequencesVocabulary.s_p_hasNext), false);

    private static SimpleListDescriptor descriptor = new SimpleListDescriptorImpl(SUBJECT, HAS_LIST, HAS_NEXT);

    private OWLOntology ontology;
    private OWLOntologyManager manager;
    private OWLDataFactory dataFactory;
    private OWLNamedIndividual individual;

    @Mock
    private OWLReasoner reasonerMock;

    @Mock
    private OwlapiAdapter adapterMock;

    private SimpleListValueDescriptor valueDescriptor = new SimpleListValueDescriptor(SUBJECT, HAS_LIST, HAS_NEXT);

    private SimpleListHandler listHandler;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final OntologyStructures realSnapshot = initRealOntology();
        this.ontology = spy(realSnapshot.getOntology());
        this.manager = spy(realSnapshot.getOntologyManager());
        this.dataFactory = realSnapshot.getDataFactory();
        when(adapterMock.getLanguage()).thenReturn("en");
        // This snapshot contains the spied on objects
        final OntologyStructures snapshotToUse = new OntologyStructures(ontology, manager, dataFactory, reasonerMock);
        this.listHandler = new SimpleListHandler(snapshotToUse, adapterMock);
        this.individual = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
    }

    private OntologyStructures initRealOntology() throws Exception {
        final OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        final OWLOntology ontology = manager.createOntology(
                IRI.create("http://krizik.felk.cvut.cz/ontologies/adapterTest"));
        return new OntologyStructures(ontology, manager, manager.getOWLDataFactory(), reasonerMock);
    }

    private static List<URI> initListItems() {
        final List<URI> lst = new ArrayList<>(10);
        for (int i = 0; i < 10; i++) {
            lst.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#ListItem_" + i));
        }
        return lst;
    }

    @Test
    public void loadListReturnsEmptyListWhenNoListHeadIsFound() throws Exception {
        final Collection<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertTrue(result.isEmpty());
    }

    @Test
    public void loadsListWithHeadOnly() throws Exception {
        final List<URI> list = LIST_ITEMS.subList(0, 1);
        persistList(list);
        final List<Axiom<NamedResource>> result = listHandler.loadList(descriptor);
        assertEquals(list.size(), result.size());
        for (int i = 0; i < list.size(); i++) {
            assertEquals(HAS_LIST, result.get(i).getAssertion());
            assertEquals(list.get(i), result.get(i).getValue().getValue().getIdentifier());
        }
    }

    private void persistList(List<URI> items) {
        assert items.size() > 0;
        final OWLObjectProperty hasList = dataFactory
                .getOWLObjectProperty(IRI.create(SequencesVocabulary.s_p_hasListProperty));
        final OWLObjectProperty hasNext = dataFactory.getOWLObjectProperty(IRI.create(SequencesVocabulary.s_p_hasNext));
        manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(hasList, individual,
                dataFactory.getOWLNamedIndividual(IRI.create(items.get(0)))));
        for (int i = 1; i < items.size(); i++) {
            manager.addAxiom(ontology,
                    dataFactory.getOWLObjectPropertyAssertionAxiom(hasNext, dataFactory.getOWLNamedIndividual(
                                    IRI.create(items.get(i - 1))),
                            dataFactory.getOWLNamedIndividual(IRI.create(items.get(i)))));

        }
    }

    @Test
    public void loadsListWithMultipleItems() throws Exception {
        persistList(LIST_ITEMS);
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
        persistList(LIST_ITEMS.subList(0, 5));
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
