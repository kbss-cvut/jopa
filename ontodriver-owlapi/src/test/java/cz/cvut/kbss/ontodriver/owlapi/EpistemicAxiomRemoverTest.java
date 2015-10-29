package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.net.URI;
import java.util.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyList;
import static org.mockito.Mockito.*;

public class EpistemicAxiomRemoverTest {

    private static final NamedResource SUBJECT = NamedResource.create("http://krizik.felk.cvut.cz/jopa#Individual");

    private OWLOntology ontology;

    private OWLOntologyManager manager;

    private OWLDataFactory dataFactory;

    @Mock
    private OwlapiAdapter adapterMock;

    private OWLNamedIndividual individual;
    private AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);

    private EpistemicAxiomRemover axiomRemover;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(adapterMock.getLanguage()).thenReturn("en");
        final OntologySnapshot snapshot = TestUtils.initRealOntology(null);
        this.ontology = snapshot.getOntology();
        this.manager = snapshot.getOntologyManager();
        this.dataFactory = snapshot.getDataFactory();
        this.axiomRemover = new EpistemicAxiomRemover(adapterMock, snapshot);
        this.individual = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
    }

    @Test
    public void removeDoesNothingWhenNoMatchingValuesAreFound() throws Exception {
        final Assertion clsAssertion = Assertion.createClassAssertion(false);
        descriptor.addAssertion(clsAssertion);
        axiomRemover.remove(descriptor);
        verify(adapterMock, never()).addTransactionalChanges(anyList());
    }

    @Test
    public void removeAxiomsByDescriptorWithClassAssertions() throws Exception {
        final Assertion clsAssertion = Assertion.createClassAssertion(false);
        descriptor.addAssertion(clsAssertion);
        initClassAssertions();
        axiomRemover.remove(descriptor);
        assertTrue(ontology.getClassAssertionAxioms(individual).isEmpty());
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock).addTransactionalChanges(captor.capture());
        final List<?> changes = captor.getValue();
        for (Object change : changes) {
            assertTrue(change instanceof RemoveAxiom);
            final RemoveAxiom ax = (RemoveAxiom) change;
            assertTrue(ax.getAxiom() instanceof OWLClassAssertionAxiom);
        }
    }

    private void initClassAssertions() {
        final OWLClass clsOne = dataFactory.getOWLClass(IRI.create("http://krizik.felk.cvut.cz/jopa#OWLClassA"));
        final OWLClass clsTwo = dataFactory.getOWLClass(IRI.create("http://krizik.felk.cvut.cz/jopa#OWLClassB"));
        manager.addAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(clsOne, individual));
        manager.addAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(clsTwo, individual));
    }

    @Test
    public void removeAxiomsByDescriptorWithDataPropertyAssertions() throws Exception {
        final Assertion dpAssertion = Assertion
                .createDataPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#dataProperty"), false);
        final OWLDataProperty odp = dataFactory.getOWLDataProperty(IRI.create(dpAssertion.getIdentifier()));
        descriptor.addAssertion(dpAssertion);
        initDataPropertyAssertions(odp);
        final int count = EntitySearcher.getDataPropertyValues(individual, odp, ontology).size();
        axiomRemover.remove(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock).addTransactionalChanges(captor.capture());
        final List<?> changes = captor.getValue();
        assertEquals(count, changes.size());
        verifyRemoveAxioms(odp, OWLDataPropertyAssertionAxiom.class, changes);
        ;
    }

    private void initDataPropertyAssertions(OWLDataProperty... dataProperties) {
        for (OWLDataProperty dp : dataProperties) {
            manager.addAxiom(ontology, dataFactory.getOWLDataPropertyAssertionAxiom(dp, individual, 117));
            manager.addAxiom(ontology, dataFactory.getOWLDataPropertyAssertionAxiom(dp, individual, 87));
        }
    }

    private void verifyRemoveAxioms(OWLProperty property, Class<? extends OWLPropertyAssertionAxiom> assertionType,
                                    Collection<?> changes) {
        for (Object change : changes) {
            assertTrue(change instanceof RemoveAxiom);
            final RemoveAxiom ax = (RemoveAxiom) change;
            assertTrue(assertionType.isAssignableFrom(ax.getAxiom().getClass()));
            final OWLPropertyAssertionAxiom removed = (OWLPropertyAssertionAxiom) ax.getAxiom();
            assertEquals(property, removed.getProperty());
        }
    }

    @Test
    public void removeAxiomsByDescriptorWithObjectPropertyAssertions() throws Exception {
        final Assertion opAssertion = Assertion
                .createObjectPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#objectProperty"), false);
        final OWLObjectProperty oop = dataFactory.getOWLObjectProperty(IRI.create(opAssertion.getIdentifier()));
        descriptor.addAssertion(opAssertion);
        initObjectPropertyAssertions(oop);
        final int count = EntitySearcher.getObjectPropertyValues(individual, oop, ontology).size();
        axiomRemover.remove(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock).addTransactionalChanges(captor.capture());
        final List<?> changes = captor.getValue();
        assertEquals(count, changes.size());
        verifyRemoveAxioms(oop, OWLObjectPropertyAssertionAxiom.class, changes);
    }

    private void initObjectPropertyAssertions(OWLObjectProperty... objectProperties) {
        final OWLNamedIndividual value = dataFactory
                .getOWLNamedIndividual(IRI.create("http://krizik.felk.cvut.cz/jopa#IndividualTwo"));
        for (OWLObjectProperty op : objectProperties) {
            manager.addAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(op, individual, value));
        }
    }

    @Test
    public void removeRetainsValuesOfAssertionsNotPresentInDescriptor() throws Exception {
        final Assertion opAssertion = Assertion
                .createObjectPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#objectProperty"), false);
        final Assertion opAssertionTwo = Assertion
                .createObjectPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#objectPropertyTwo"), false);
        descriptor.addAssertion(opAssertion);
        final OWLObjectProperty opOne = dataFactory.getOWLObjectProperty(IRI.create(opAssertion.getIdentifier()));
        final OWLObjectProperty opTwo = dataFactory.getOWLObjectProperty(IRI.create(opAssertionTwo.getIdentifier()));
        initObjectPropertyAssertions(opOne, opTwo);
        final int countTwo = EntitySearcher.getObjectPropertyValues(individual, opTwo, ontology).size();
        axiomRemover.remove(descriptor);
        final int resultCountOne = EntitySearcher.getObjectPropertyValues(individual, opOne, ontology).size();
        final int resultCountTwo = EntitySearcher.getObjectPropertyValues(individual, opTwo, ontology).size();
        assertEquals(0, resultCountOne);
        assertEquals(countTwo, resultCountTwo);
    }

    @Test
    public void removeAxiomsByDescriptorWithAnnotationPropertyAssertions() throws Exception {
        final Assertion apAssertion = Assertion.createAnnotationPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/jopa#annotationProperty"), false);
        final OWLAnnotationProperty oap = dataFactory.getOWLAnnotationProperty(IRI.create(apAssertion.getIdentifier()));
        descriptor.addAssertion(apAssertion);
        initAnnotationAssertions(oap);
        axiomRemover.remove(descriptor);
        assertEquals(0, EntitySearcher.getAnnotationAssertionAxioms(individual.getIRI(), ontology).size());
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock).addTransactionalChanges(captor.capture());
        final List<?> changes = captor.getValue();
        verifyAnnotationRemoveAxioms(oap, changes);
    }

    private void initAnnotationAssertions(OWLAnnotationProperty... annotationProperties) {
        for (OWLAnnotationProperty ap : annotationProperties) {
            manager.addAxiom(ontology, dataFactory
                    .getOWLAnnotationAssertionAxiom(ap, individual.getIRI(), dataFactory.getOWLLiteral(117)));
            manager.addAxiom(ontology, dataFactory
                    .getOWLAnnotationAssertionAxiom(ap, individual.getIRI(), dataFactory.getOWLLiteral("String")));
        }
    }

    private void verifyAnnotationRemoveAxioms(OWLProperty property, Collection<?> changes) {
        for (Object change : changes) {
            assertTrue(change instanceof RemoveAxiom);
            final RemoveAxiom ax = (RemoveAxiom) change;
            assertTrue(OWLAnnotationAssertionAxiom.class.isAssignableFrom(ax.getAxiom().getClass()));
            final OWLAnnotationAssertionAxiom removed = (OWLAnnotationAssertionAxiom) ax.getAxiom();
            assertEquals(property, removed.getProperty());
        }
    }

    @Test
    public void removeCombinationOfDataObjectAndAnnotationPropertiesAndClassAssertion() throws Exception {
        final Set<OWLPropertyExpression> removed = initForCombinedTest();
        axiomRemover.remove(descriptor);
        final ArgumentCaptor<List> captor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock).addTransactionalChanges(captor.capture());
        final List<?> changes = captor.getValue();
        for (Object change : changes) {
            final RemoveAxiom ax = (RemoveAxiom) change;
            final OWLPropertyExpression op;
            if (!(ax.getAxiom() instanceof OWLClassAssertionAxiom)) {
                if (ax.getAxiom() instanceof OWLAnnotationAssertionAxiom) {
                    op = ((OWLAnnotationAssertionAxiom) ax.getAxiom()).getProperty();
                } else {
                    op = ((OWLPropertyAssertionAxiom) ax.getAxiom()).getProperty();
                }
                assertTrue(removed.contains(op));
            }
        }
    }

    private Set<OWLPropertyExpression> initForCombinedTest() {
        final Assertion clsAssertion = Assertion.createClassAssertion(false);
        descriptor.addAssertion(clsAssertion);
        initClassAssertions();
        final Assertion dpAssertion = Assertion
                .createDataPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#dataProperty"), false);
        final OWLDataProperty odp = dataFactory.getOWLDataProperty(IRI.create(dpAssertion.getIdentifier()));
        descriptor.addAssertion(dpAssertion);
        initDataPropertyAssertions(odp);
        final Assertion opAssertion = Assertion
                .createObjectPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#objectProperty"), false);
        final Assertion opAssertionTwo = Assertion
                .createObjectPropertyAssertion(URI.create("http://krizik.felk.cvut.cz/jopa#objectPropertyTwo"), false);
        descriptor.addAssertion(opAssertion);
        final OWLObjectProperty opOne = dataFactory.getOWLObjectProperty(IRI.create(opAssertion.getIdentifier()));
        final OWLObjectProperty opTwo = dataFactory.getOWLObjectProperty(IRI.create(opAssertionTwo.getIdentifier()));
        initObjectPropertyAssertions(opOne, opTwo);
        final Assertion ap = Assertion.createAnnotationPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/jopa#annotationProperty"), false);
        final OWLAnnotationProperty oap = dataFactory.getOWLAnnotationProperty(IRI.create(ap.getIdentifier()));
        descriptor.addAssertion(ap);
        initAnnotationAssertions(oap);
        return new HashSet<>(Arrays.asList(odp, opOne, oap));
    }
}