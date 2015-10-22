package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.NodeFactory;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class MainAxiomLoaderTest {

    private static final NamedResource SUBJECT = NamedResource.create("http://krizik.felk.cvut.cz/Individual");

    @Mock
    private OwlapiAdapter adapterMock;

    @Mock
    private OWLOntology ontologyMock;

    @Mock
    private OWLOntologyManager managerMock;

    @Mock
    private OWLReasoner reasonerMock;

    private OWLDataFactory dataFactory = new OWLDataFactoryImpl();

    private MainAxiomLoader axiomLoader;

    private OWLNamedIndividual individual;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final OntologyStructures snapshot = new OntologyStructures(ontologyMock, managerMock, dataFactory,
                reasonerMock);
        this.axiomLoader = new MainAxiomLoader(adapterMock, snapshot);
        this.individual = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
        when(ontologyMock.containsIndividualInSignature(individual.getIRI())).thenReturn(true);
    }

    @Test
    public void returnsEmptyCollectionWhenIndividualIsNotInOntology() throws Exception {
        when(ontologyMock.containsIndividualInSignature(individual.getIRI())).thenReturn(false);
        assertTrue(axiomLoader.findAxioms(descriptor()).isEmpty());
        verify(ontologyMock, never()).getDataPropertyAssertionAxioms(any(OWLIndividual.class));
        verify(ontologyMock, never()).getObjectPropertyAssertionAxioms(any(OWLIndividual.class));
        verify(ontologyMock, never()).getAnnotationAssertionAxioms(any(IRI.class));
    }

    @Test
    public void loadsExplicitDataPropertyValuesForAssertions() throws Exception {
        final URI assertionProperty = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion dp = Assertion.createDataPropertyAssertion(assertionProperty, false);
        final Set<OWLDataPropertyAssertionAxiom> axioms = new HashSet<>();
        axioms.add(dataFactory
                .getOWLDataPropertyAssertionAxiom(dataFactory.getOWLDataProperty(IRI.create(assertionProperty)),
                        individual, 158));
        axioms.add(dataFactory.getOWLDataPropertyAssertionAxiom(
                dataFactory.getOWLDataProperty(IRI.create("http://krizik.felk.cvut.cz/PropertyTwo")), individual, 200));
        when(ontologyMock.getDataPropertyAssertionAxioms(individual)).thenReturn(axioms);

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(dp));
        verify(ontologyMock).getDataPropertyAssertionAxioms(individual);
        for (Axiom<?> ax : result) {
            assertEquals(dp, ax.getAssertion());
        }
    }

    private AxiomDescriptor descriptor(Assertion... assertions) {
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        for (Assertion a : assertions) {
            descriptor.addAssertion(a);
        }
        return descriptor;
    }

    @Test
    public void loadsExplicitObjectPropertyValuesForAssertions() throws Exception {
        final URI assertionPropertyOne = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion opOne = Assertion.createObjectPropertyAssertion(assertionPropertyOne, false);
        final URI assertionPropertyTwo = URI.create("http://krizik.felk.cvut.cz/PropertyTwo");
        final Assertion opTwo = Assertion.createObjectPropertyAssertion(assertionPropertyTwo, false);
        final OWLNamedIndividual otherIndividual = dataFactory
                .getOWLNamedIndividual(IRI.create("http://krizik.felk.cvut.cz/OtherIndividual"));
        final Set<OWLObjectPropertyAssertionAxiom> axioms = new HashSet<>();
        axioms.add(dataFactory.getOWLObjectPropertyAssertionAxiom(
                dataFactory.getOWLObjectProperty(IRI.create(assertionPropertyOne)), individual, otherIndividual));
        axioms.add(dataFactory
                .getOWLObjectPropertyAssertionAxiom(dataFactory.getOWLObjectProperty(IRI.create(assertionPropertyTwo)),
                        individual, otherIndividual));
        axioms.add(dataFactory.getOWLObjectPropertyAssertionAxiom(
                dataFactory.getOWLObjectProperty(IRI.create("http://krizik.felk.cvut.cz/otherProperty")), individual,
                otherIndividual));
        when(ontologyMock.getObjectPropertyAssertionAxioms(individual)).thenReturn(axioms);

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(opOne, opTwo));
        verify(ontologyMock).getObjectPropertyAssertionAxioms(individual);
        for (Axiom<?> ax : result) {
            assertTrue(ax.getAssertion().equals(opOne) || ax.getAssertion().equals(opTwo));
        }
    }

    @Test
    public void loadsExplicitAnnotationPropertyValuesForAssertions() throws Exception {
        final URI assertionProperty = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion dp = Assertion.createAnnotationPropertyAssertion(assertionProperty, false);
        final Set<OWLAnnotationAssertionAxiom> axioms = new HashSet<>();
        axioms.add(dataFactory
                .getOWLAnnotationAssertionAxiom(dataFactory.getOWLAnnotationProperty(IRI.create(assertionProperty)),
                        individual.getIRI(), dataFactory.getOWLLiteral(158)));
        axioms.add(dataFactory.getOWLAnnotationAssertionAxiom(
                dataFactory.getOWLAnnotationProperty(IRI.create("http://krizik.felk.cvut.cz/PropertyTwo")),
                individual.getIRI(), dataFactory.getOWLLiteral(200)));
        when(ontologyMock.getAnnotationAssertionAxioms(individual.getIRI())).thenReturn(axioms);

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(dp));
        verify(ontologyMock).getAnnotationAssertionAxioms(individual.getIRI());
        for (Axiom<?> ax : result) {
            assertEquals(dp, ax.getAssertion());
        }
    }

    @Test
    public void loadsExplicitTypesWhenClassAssertionIsInTheDescriptor() throws Exception {
        final Assertion classAssertion = Assertion.createClassAssertion(false);
        final TypesHandler typesMock = mock(TypesHandler.class);
        when(adapterMock.getTypesHandler()).thenReturn(typesMock);
        when(typesMock.getTypes(SUBJECT, null, false)).thenReturn(Collections
                .singleton(new AxiomImpl<>(SUBJECT, classAssertion, new Value<>(URI.create("http://typeA")))));

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(classAssertion));
        verify(typesMock).getTypes(SUBJECT, null, false);
        assertEquals(1, result.size());
        assertEquals(classAssertion, result.iterator().next().getAssertion());
    }

    @Test
    public void loadsExplicitValuesForUntypedAssertion() throws Exception {
        final URI assertionUri = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion assertion = Assertion.createPropertyAssertion(assertionUri, false);
        when(ontologyMock.getDataPropertyAssertionAxioms(individual))
                .thenReturn(Collections.singleton(dataFactory.getOWLDataPropertyAssertionAxiom(
                        dataFactory.getOWLDataProperty(IRI.create("http://krizik.felk.cvut.cz/PropertyOne")),
                        individual, 200)));
        when(ontologyMock.getAnnotationAssertionAxioms(individual.getIRI()))
                .thenReturn(Collections.singleton(dataFactory.getOWLAnnotationAssertionAxiom(
                        dataFactory.getOWLAnnotationProperty(IRI.create("http://krizik.felk.cvut.cz/PropertyOne")),
                        individual.getIRI(), dataFactory.getOWLLiteral(200))));
        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(assertion));
        assertFalse(result.isEmpty());
        verify(ontologyMock).getDataPropertyAssertionAxioms(individual);
        verify(ontologyMock).getObjectPropertyAssertionAxioms(individual);
        verify(ontologyMock).getAnnotationAssertionAxioms(individual.getIRI());
    }

    @Test
    public void loadsInferredDataPropertyValues() throws Exception {
        final URI dpOneUri = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion dpOne = Assertion.createDataPropertyAssertion(dpOneUri, true);
        final OWLDataProperty owlDpOne = dataFactory.getOWLDataProperty(IRI.create(dpOneUri));
        final Set<OWLLiteral> valuesOne = new HashSet<>();
        valuesOne.add(dataFactory.getOWLLiteral(158));
        valuesOne.add(dataFactory.getOWLLiteral(159));
        when(reasonerMock.getDataPropertyValues(individual, owlDpOne)).thenReturn(valuesOne);
        final URI dpTwoUri = URI.create("http://krizik.felk.cvut.cz/PropertyTwo");
        final Assertion dpTwo = Assertion.createDataPropertyAssertion(dpTwoUri, true);
        final OWLDataProperty owlDpTwo = dataFactory.getOWLDataProperty(IRI.create(dpTwoUri));
        final Set<OWLLiteral> valuesTwo = new HashSet<>();
        valuesTwo.add(dataFactory.getOWLLiteral(true));
        when(reasonerMock.getDataPropertyValues(individual, owlDpTwo)).thenReturn(valuesTwo);

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(dpOne, dpTwo));
        assertEquals(valuesOne.size() + valuesTwo.size(), result.size());
        for (Axiom<?> ax : result) {
            assertTrue(ax.getAssertion().equals(dpOne) || ax.getAssertion().equals(dpTwo));
        }
    }

    @Test
    public void loadsInferredObjectPropertyValues() throws Exception {
        final URI opUri = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion op = Assertion.createObjectPropertyAssertion(opUri, true);
        final OWLObjectProperty owlOp = dataFactory.getOWLObjectProperty(IRI.create(opUri));
        final NodeSet<OWLNamedIndividual> individuals = new OWLNamedIndividualNodeSet(
                dataFactory.getOWLNamedIndividual(IRI.create("http://krizik.felk.cvut.cz/IndividialOne")));
        when(reasonerMock.getObjectPropertyValues(individual, owlOp)).thenReturn(individuals);

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(op));
        for (Axiom<?> ax : result) {
            assertEquals(op, ax.getAssertion());
        }
    }

    @Test
    public void loadsInferredTypes() throws Exception {
        final Assertion classAssertion = Assertion.createClassAssertion(true);
        final Set<Axiom<URI>> types = new HashSet<>();
        types.add(new AxiomImpl<>(SUBJECT, classAssertion,
                new Value<>(URI.create("http://krizik.felk.cvut.cz/TypeOne"))));
        types.add(new AxiomImpl<>(SUBJECT, classAssertion,
                new Value<>(URI.create("http://krizik.felk.cvut.cz/TypeTwo"))));
        final TypesHandler typesMock = mock(TypesHandler.class);
        when(typesMock.getTypes(SUBJECT, null, true)).thenReturn(types);
        when(adapterMock.getTypesHandler()).thenReturn(typesMock);

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(classAssertion));
        assertEquals(types.size(), result.size());
        for (Axiom<?> ax : result) {
            assertEquals(classAssertion, ax.getAssertion());
        }
    }

    @Test(expected = ReasonerNotAvailableException.class)
    public void throwsExceptionWhenReasonerIsNotAvailableForInferredAssertions() throws Exception {
        final URI opUri = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion op = Assertion.createObjectPropertyAssertion(opUri, true);
        final OntologyStructures snapshot = new OntologyStructures(ontologyMock, managerMock, dataFactory, null);
        final MainAxiomLoader loader = new MainAxiomLoader(adapterMock, snapshot);

        loader.findAxioms(descriptor(op));
    }

    @Test
    public void skipsExplicitAssertionValueIfThereIsTheSameAssertionAlsoWithInference() throws Exception {
        final URI opUri = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion opAsserted = Assertion.createObjectPropertyAssertion(opUri, false);
        final Assertion opInferred = Assertion.createObjectPropertyAssertion(opUri, true);
        final OWLObjectProperty owlOp = dataFactory.getOWLObjectProperty(IRI.create(opUri));
        final Set<Node<OWLNamedIndividual>> indSet = new HashSet<>();
        final OWLNamedIndividual commonInd = dataFactory.getOWLNamedIndividual(
                IRI.create("http://krizik.felk.cvut.cz/IndividialOne"));
        indSet.add(NodeFactory.getOWLNamedIndividualNode(commonInd));
        indSet.add(NodeFactory.getOWLNamedIndividualNode(
                dataFactory.getOWLNamedIndividual(IRI.create("http://krizik.felk.cvut.cz/IndividialTwo"))));
        final NodeSet<OWLNamedIndividual> individuals = new OWLNamedIndividualNodeSet(indSet);
        when(reasonerMock.getObjectPropertyValues(individual, owlOp)).thenReturn(individuals);
        final Set<OWLObjectPropertyAssertionAxiom> axioms = Collections
                .singleton(dataFactory.getOWLObjectPropertyAssertionAxiom(
                        dataFactory.getOWLObjectProperty(IRI.create(opUri)), individual, commonInd));
        when(ontologyMock.getObjectPropertyAssertionAxioms(individual)).thenReturn(axioms);

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(opAsserted, opInferred));
        assertEquals(indSet.size(), result.size());
        for (Axiom ax : result) {
            assertEquals(opInferred, ax.getAssertion());
        }
    }
}