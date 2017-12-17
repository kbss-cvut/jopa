/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
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
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.Assert.*;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;

public class MainAxiomLoaderTest {

    private static final NamedResource SUBJECT = NamedResource.create("http://krizik.felk.cvut.cz/Individual");
    private static final String RDFS_LABEL = "http://www.w3.org/2000/01/rdf-schema#label";
    private static final String LANG = "en";

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
        final OntologySnapshot snapshot = new OntologySnapshot(ontologyMock, managerMock, dataFactory,
                reasonerMock);
        when(adapterMock.getLanguage()).thenReturn(LANG);
        this.axiomLoader = new MainAxiomLoader(adapterMock, snapshot);
        this.individual = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
        when(ontologyMock.containsIndividualInSignature(individual.getIRI())).thenReturn(true);
    }

    @Test
    public void returnsEmptyCollectionWhenIndividualIsNotInOntology() {
        when(ontologyMock.containsIndividualInSignature(individual.getIRI())).thenReturn(false);
        assertTrue(axiomLoader.findAxioms(descriptor()).isEmpty());
        verify(ontologyMock, never()).dataPropertyAssertionAxioms(any(OWLIndividual.class));
        verify(ontologyMock, never()).objectPropertyAssertionAxioms(any(OWLIndividual.class));
        verify(ontologyMock, never()).annotationAssertionAxioms(any(IRI.class));
    }

    @Test
    public void loadsExplicitDataPropertyValuesForAssertions() {
        final URI assertionProperty = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion dp = Assertion.createDataPropertyAssertion(assertionProperty, false);
        final Set<OWLDataPropertyAssertionAxiom> axioms = new HashSet<>();
        axioms.add(dataFactory
                .getOWLDataPropertyAssertionAxiom(dataFactory.getOWLDataProperty(IRI.create(assertionProperty)),
                        individual, 158));
        axioms.add(dataFactory.getOWLDataPropertyAssertionAxiom(
                dataFactory.getOWLDataProperty(IRI.create("http://krizik.felk.cvut.cz/PropertyTwo")), individual, 200));
        when(ontologyMock.dataPropertyAssertionAxioms(individual)).thenReturn(axioms.stream());
        when(ontologyMock.annotationAssertionAxioms(any())).thenReturn(Stream.empty());
        when(ontologyMock.objectPropertyAssertionAxioms(any())).thenReturn(Stream.empty());

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(dp));
        verify(ontologyMock).dataPropertyAssertionAxioms(individual);
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
    public void loadsExplicitObjectPropertyValuesForAssertions() {
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
        when(ontologyMock.objectPropertyAssertionAxioms(individual)).thenReturn(axioms.stream());
        when(ontologyMock.dataPropertyAssertionAxioms(any())).thenReturn(Stream.empty());
        when(ontologyMock.annotationAssertionAxioms(any())).thenReturn(Stream.empty());

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(opOne, opTwo));
        verify(ontologyMock).objectPropertyAssertionAxioms(individual);
        for (Axiom<?> ax : result) {
            assertTrue(ax.getAssertion().equals(opOne) || ax.getAssertion().equals(opTwo));
        }
    }

    @Test
    public void loadsExplicitAnnotationPropertyValuesForAssertions() {
        final URI assertionProperty = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion dp = Assertion.createAnnotationPropertyAssertion(assertionProperty, false);
        final Set<OWLAnnotationAssertionAxiom> axioms = new HashSet<>();
        axioms.add(dataFactory
                .getOWLAnnotationAssertionAxiom(dataFactory.getOWLAnnotationProperty(IRI.create(assertionProperty)),
                        individual.getIRI(), dataFactory.getOWLLiteral(158)));
        axioms.add(dataFactory.getOWLAnnotationAssertionAxiom(
                dataFactory.getOWLAnnotationProperty(IRI.create("http://krizik.felk.cvut.cz/PropertyTwo")),
                individual.getIRI(), dataFactory.getOWLLiteral(200)));
        when(ontologyMock.annotationAssertionAxioms(individual.getIRI())).thenReturn(axioms.stream());
        when(ontologyMock.dataPropertyAssertionAxioms(any())).thenReturn(Stream.empty());
        when(ontologyMock.objectPropertyAssertionAxioms(any())).thenReturn(Stream.empty());

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(dp));
        verify(ontologyMock).annotationAssertionAxioms(individual.getIRI());
        for (Axiom<?> ax : result) {
            assertEquals(dp, ax.getAssertion());
        }
    }

    @Test
    public void loadsExplicitTypesWhenClassAssertionIsInTheDescriptor() {
        final Assertion classAssertion = Assertion.createClassAssertion(false);
        final TypesHandler typesMock = mock(TypesHandler.class);
        when(adapterMock.getTypesHandler()).thenReturn(typesMock);
        when(typesMock.getTypes(SUBJECT, null, false)).thenReturn(Collections
                .singleton(new AxiomImpl<>(SUBJECT, classAssertion, new Value<>(URI.create("http://typeA")))));
        when(ontologyMock.annotationAssertionAxioms(any())).thenReturn(Stream.empty());
        when(ontologyMock.dataPropertyAssertionAxioms(any())).thenReturn(Stream.empty());
        when(ontologyMock.objectPropertyAssertionAxioms(any())).thenReturn(Stream.empty());

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(classAssertion));
        verify(typesMock).getTypes(SUBJECT, null, false);
        assertEquals(1, result.size());
        assertEquals(classAssertion, result.iterator().next().getAssertion());
    }

    @Test
    public void loadsExplicitValuesForUntypedAssertion() {
        final URI assertionUri = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion assertion = Assertion.createPropertyAssertion(assertionUri, false);
        when(ontologyMock.dataPropertyAssertionAxioms(individual))
                .thenReturn(Stream.of(dataFactory.getOWLDataPropertyAssertionAxiom(
                        dataFactory.getOWLDataProperty(IRI.create("http://krizik.felk.cvut.cz/PropertyOne")),
                        individual, 200)));
        when(ontologyMock.annotationAssertionAxioms(individual.getIRI()))
                .thenReturn(Stream.of(dataFactory.getOWLAnnotationAssertionAxiom(
                        dataFactory.getOWLAnnotationProperty(IRI.create("http://krizik.felk.cvut.cz/PropertyOne")),
                        individual.getIRI(), dataFactory.getOWLLiteral(200))));
        when(ontologyMock.dataPropertyAssertionAxioms(any())).thenReturn(Stream.empty());
        when(ontologyMock.objectPropertyAssertionAxioms(any())).thenReturn(Stream.empty());

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(assertion));
        assertFalse(result.isEmpty());
        verify(ontologyMock).dataPropertyAssertionAxioms(individual);
        verify(ontologyMock).objectPropertyAssertionAxioms(individual);
        verify(ontologyMock).annotationAssertionAxioms(individual.getIRI());
    }

    @Test
    public void loadsInferredDataPropertyValues() {
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
    public void loadsInferredObjectPropertyValues() {
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
    public void loadsInferredTypes() {
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
    public void throwsExceptionWhenReasonerIsNotAvailableForInferredAssertions() {
        final URI opUri = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion op = Assertion.createObjectPropertyAssertion(opUri, true);
        final OntologySnapshot snapshot = new OntologySnapshot(ontologyMock, managerMock, dataFactory, null);
        final MainAxiomLoader loader = new MainAxiomLoader(adapterMock, snapshot);

        loader.findAxioms(descriptor(op));
    }

    @Test
    public void skipsExplicitAssertionValueIfThereIsTheSameAssertionAlsoWithInference() {
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
        final Stream<OWLObjectPropertyAssertionAxiom> axioms = Stream.of(dataFactory.getOWLObjectPropertyAssertionAxiom(
                        dataFactory.getOWLObjectProperty(IRI.create(opUri)), individual, commonInd));
        when(ontologyMock.objectPropertyAssertionAxioms(individual)).thenReturn(axioms);
        when(ontologyMock.dataPropertyAssertionAxioms(any())).thenReturn(Stream.empty());
        when(ontologyMock.annotationAssertionAxioms(any())).thenReturn(Stream.empty());

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(opAsserted, opInferred));
        assertEquals(indSet.size(), result.size());
        for (Axiom ax : result) {
            assertEquals(opInferred, ax.getAssertion());
        }
    }

    @Test
    public void loadsStringLiteralValueForExplicitAnnotationPropertyWithCorrectLanguageTag() {
        initExplicitAnnotationPropertyStringsWithLanguageTag();

        final Collection<Axiom<?>> result = axiomLoader
                .findAxioms(descriptor(Assertion.createAnnotationPropertyAssertion(URI.create(RDFS_LABEL), false)));
        checkLoadedAxiomsForStringValue(result, "a");
    }

    private void initExplicitAnnotationPropertyStringsWithLanguageTag() {
        final Set<OWLAnnotationAssertionAxiom> axioms = new HashSet<>();
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
        final OWLAnnotationProperty property = dataFactory.getOWLAnnotationProperty(IRI.create(RDFS_LABEL));
        axioms.add(dataFactory.getOWLAnnotationAssertionAxiom(property, individual.getIRI(),
                dataFactory.getOWLLiteral("a", LANG)));
        axioms.add(dataFactory.getOWLAnnotationAssertionAxiom(property, individual.getIRI(),
                dataFactory.getOWLLiteral("b", "cs")));
        when(ontologyMock.annotationAssertionAxioms(individual.getIRI())).thenReturn(axioms.stream());
        when(ontologyMock.dataPropertyAssertionAxioms(any())).thenReturn(Stream.empty());
        when(ontologyMock.objectPropertyAssertionAxioms(any())).thenReturn(Stream.empty());
    }

    private void checkLoadedAxiomsForStringValue(Collection<Axiom<?>> result, String expected) {
        assertEquals(1, result.size());
        final Axiom<?> ax = result.iterator().next();
        assertEquals(expected, ax.getValue().getValue());
    }

    @Test
    public void loadsStringLiteralValueForExplicitDataPropertyWithCorrectLanguageTag() {
        final String propertyUri = "http://krizik.felk.cvut.cz/dataPropertyOne";
        initExplicitDataPropertyStringsWithLanguageTag(propertyUri);

        final Collection<Axiom<?>> result = axiomLoader
                .findAxioms(descriptor(Assertion.createDataPropertyAssertion(URI.create(propertyUri), false)));
        checkLoadedAxiomsForStringValue(result, "a");
    }

    private void initExplicitDataPropertyStringsWithLanguageTag(String propertyUri) {
        final Set<OWLDataPropertyAssertionAxiom> axioms = new HashSet<>();
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
        final OWLDataProperty property = dataFactory.getOWLDataProperty(IRI.create(propertyUri));
        axioms.add(dataFactory
                .getOWLDataPropertyAssertionAxiom(property, individual, dataFactory.getOWLLiteral("a", LANG)));
        axioms.add(dataFactory
                .getOWLDataPropertyAssertionAxiom(property, individual, dataFactory.getOWLLiteral("b", "cs")));
        when(ontologyMock.dataPropertyAssertionAxioms(individual)).thenReturn(axioms.stream());
        when(ontologyMock.annotationAssertionAxioms(any())).thenReturn(Stream.empty());
        when(ontologyMock.objectPropertyAssertionAxioms(any())).thenReturn(Stream.empty());
    }

    @Test
    public void loadsStringLiteralValueForInferredDataPropertyWithCorrectLanguageTag() {
        final OWLDataProperty dp = dataFactory.getOWLDataProperty(IRI.create(RDFS_LABEL));
        final Set<OWLLiteral> values = new HashSet<>();
        values.add(dataFactory.getOWLLiteral("a", LANG));
        values.add(dataFactory.getOWLLiteral("b", "cs"));
        when(reasonerMock.getDataPropertyValues(individual, dp)).thenReturn(values);

        final Collection<Axiom<?>> result = axiomLoader
                .findAxioms(descriptor(Assertion.createDataPropertyAssertion(URI.create(RDFS_LABEL), true)));
        checkLoadedAxiomsForStringValue(result, "a");
    }

    @Test
    public void loadsStringLiteralWithCorrectLanguageTagWhenItIsSpecifiedInExplicitDataPropertyAssertion() {
        final String propertyUri = "http://krizik.felk.cvut.cz/dataPropertyOne";
        initExplicitDataPropertyStringsWithLanguageTag(propertyUri);
        final Assertion dpa = Assertion.createDataPropertyAssertion(URI.create(propertyUri), "cs", false);
        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(dpa));
        checkLoadedAxiomsForStringValue(result, "b");
    }

    @Test
    public void loadsStringLiteralWithCorrectLanguageTagWhenItIsSpecifiedInExplicitAnnotationPropertyAssertion() {
        initExplicitAnnotationPropertyStringsWithLanguageTag();
        final Assertion apa = Assertion.createAnnotationPropertyAssertion(URI.create(RDFS_LABEL), "cs", false);
        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(apa));
        checkLoadedAxiomsForStringValue(result, "b");
    }

    @Test
    public void loadsStringLiteralWithAllLanguagesWhenLanguageTagIsExplicitlySetToNull() {
        initExplicitAnnotationPropertyStringsWithLanguageTag();
        final Assertion apa = Assertion.createAnnotationPropertyAssertion(URI.create(RDFS_LABEL), null, false);
        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(apa));
        assertEquals(2, result.size());
        final Set<String> values = result.stream().map(ax -> ax.getValue().stringValue()).collect(Collectors.toSet());
        assertTrue(values.contains("a"));
        assertTrue(values.contains("b"));
    }

    @Test
    public void loadsStringLiteralWithCorrectLanguageTagWhenSpecifiedOnUnspecifiedDataProperty() {
        final String propertyUri = "http://krizik.felk.cvut.cz/dataPropertyOne";
        initExplicitDataPropertyStringsWithLanguageTag(propertyUri);
        final Assertion assertion = Assertion.createUnspecifiedPropertyAssertion("cs", false);
        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(assertion));
        checkLoadedAxiomsForStringValue(result, "b");
    }

    @Test
    public void loadsStringLiteralWithCorrectLanguageTagWhenSpecifiedOnUnspecifiedAnnotationProperty() {
        initExplicitAnnotationPropertyStringsWithLanguageTag();
        final Assertion assertion = Assertion.createUnspecifiedPropertyAssertion("cs", false);
        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(assertion));
        checkLoadedAxiomsForStringValue(result, "b");
    }

    @Test
    public void loadsStringLiteralWithCorrectLanguageTagSpecifiedOnInferredDataProperty() {
        final String propertyUri = "http://krizik.felk.cvut.cz/dataPropertyOne";
        final OWLDataProperty dp = dataFactory.getOWLDataProperty(IRI.create(propertyUri));
        final Set<OWLLiteral> values = new HashSet<>();
        values.add(dataFactory.getOWLLiteral("a", LANG));
        values.add(dataFactory.getOWLLiteral("b", "cs"));
        when(reasonerMock.getDataPropertyValues(individual, dp)).thenReturn(values);
        final Assertion assertion = Assertion.createDataPropertyAssertion(URI.create(propertyUri), "cs", true);

        final Collection<Axiom<?>> result = axiomLoader.findAxioms(descriptor(assertion));
        checkLoadedAxiomsForStringValue(result, "b");
    }
}