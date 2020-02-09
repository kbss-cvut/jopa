/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.Generator;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver.owlapi.exception.ReasonerNotAvailableException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.NodeFactory;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class MainAxiomLoaderTest {

    private static final NamedResource SUBJECT = NamedResource.create("http://krizik.felk.cvut.cz/Individual");
    private static final String RDFS_LABEL = "http://www.w3.org/2000/01/rdf-schema#label";
    private static final String LANG = "en";

    @Mock
    private OwlapiAdapter adapterMock;

    @Mock
    private OWLReasoner reasonerMock;

    private OWLDataFactory dataFactory;

    private OWLOntology ontology;

    private OWLOntologyManager manager;

    private MainAxiomLoader sut;

    private OWLNamedIndividual individual;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final OntologySnapshot snapshot = TestUtils.initRealOntology(reasonerMock);
        this.ontology = snapshot.getOntology();
        this.manager = snapshot.getOntologyManager();
        this.dataFactory = snapshot.getDataFactory();
        this.sut = new MainAxiomLoader(adapterMock, snapshot);
        this.individual = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
        manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLDeclarationAxiom(individual)));
    }

    @Test
    void returnsEmptyCollectionWhenIndividualIsNotInOntology() {
        assertTrue(sut.findAxioms(descriptor()).isEmpty());
    }

    @Test
    void loadsExplicitDataPropertyValuesForAssertions() {
        final URI assertionProperty = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion dp = Assertion.createDataPropertyAssertion(assertionProperty, false);
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLDataPropertyAssertionAxiom(dataFactory.getOWLDataProperty(IRI.create(assertionProperty)),
                        individual, 158)));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLDataPropertyAssertionAxiom(dataFactory.getOWLDataProperty(IRI.create(assertionProperty)),
                        individual, 200)));

        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(dp));
        assertEquals(2, result.size());
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
    void loadsExplicitObjectPropertyValuesForAssertions() {
        final URI assertionPropertyOne = Generator.generateUri();
        final Assertion opOne = Assertion.createObjectPropertyAssertion(assertionPropertyOne, false);
        final URI assertionPropertyTwo = Generator.generateUri();
        final Assertion opTwo = Assertion.createObjectPropertyAssertion(assertionPropertyTwo, false);
        final OWLNamedIndividual otherIndividual =
                dataFactory.getOWLNamedIndividual(IRI.create(Generator.generateUri()));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLObjectPropertyAssertionAxiom(dataFactory.getOWLObjectProperty(IRI.create(assertionPropertyOne)),
                        individual, otherIndividual)));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLObjectPropertyAssertionAxiom(dataFactory.getOWLObjectProperty(IRI.create(assertionPropertyTwo)),
                        individual, otherIndividual)));

        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(opOne, opTwo));
        for (Axiom<?> ax : result) {
            assertTrue(ax.getAssertion().equals(opOne) || ax.getAssertion().equals(opTwo));
        }
    }

    @Test
    void loadsExplicitAnnotationPropertyValuesForAssertions() {
        final URI assertionProperty = Generator.generateUri();
        final Assertion dp = Assertion.createAnnotationPropertyAssertion(assertionProperty, false);
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLAnnotationAssertionAxiom(individual.getIRI(), dataFactory
                        .getOWLAnnotation(dataFactory.getOWLAnnotationProperty(IRI.create(assertionProperty)),
                                dataFactory.getOWLLiteral(158)))));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLAnnotationAssertionAxiom(individual.getIRI(), dataFactory
                        .getOWLAnnotation(dataFactory.getOWLAnnotationProperty(IRI.create(assertionProperty)),
                                dataFactory.getOWLLiteral(200)))));

        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(dp));
        for (Axiom<?> ax : result) {
            assertEquals(dp, ax.getAssertion());
        }
    }

    @Test
    void loadsExplicitTypesWhenClassAssertionIsInTheDescriptor() {
        final Assertion classAssertion = Assertion.createClassAssertion(false);
        final TypesHandler typesMock = mock(TypesHandler.class);
        when(adapterMock.getTypesHandler()).thenReturn(typesMock);
        when(typesMock.getTypes(SUBJECT, null, false)).thenReturn(Collections
                .singleton(new AxiomImpl<>(SUBJECT, classAssertion, new Value<>(URI.create("http://typeA")))));

        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(classAssertion));
        verify(typesMock).getTypes(SUBJECT, null, false);
        assertEquals(1, result.size());
        assertEquals(classAssertion, result.iterator().next().getAssertion());
    }

    @Test
    void loadsExplicitValuesForUntypedAssertion() {
        final URI assertionUri = Generator.generateUri();
        final Assertion assertion = Assertion.createPropertyAssertion(assertionUri, false);
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLDataPropertyAssertionAxiom(dataFactory.getOWLDataProperty(IRI.create(assertionUri)),
                        individual, 158)));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLDataPropertyAssertionAxiom(dataFactory.getOWLDataProperty(IRI.create(Generator.generateUri())),
                        individual, 200)));

        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(assertion));
        assertFalse(result.isEmpty());
    }

    @Test
    void loadsInferredDataPropertyValues() {
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

        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(dpOne, dpTwo));
        assertEquals(valuesOne.size() + valuesTwo.size(), result.size());
        for (Axiom<?> ax : result) {
            assertTrue(ax.getAssertion().equals(dpOne) || ax.getAssertion().equals(dpTwo));
        }
    }

    @Test
    void loadsInferredObjectPropertyValues() {
        final URI opUri = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion op = Assertion.createObjectPropertyAssertion(opUri, true);
        final OWLObjectProperty owlOp = dataFactory.getOWLObjectProperty(IRI.create(opUri));
        final NodeSet<OWLNamedIndividual> individuals = new OWLNamedIndividualNodeSet(
                dataFactory.getOWLNamedIndividual(IRI.create("http://krizik.felk.cvut.cz/IndividialOne")));
        when(reasonerMock.getObjectPropertyValues(individual, owlOp)).thenReturn(individuals);

        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(op));
        for (Axiom<?> ax : result) {
            assertEquals(op, ax.getAssertion());
        }
    }

    @Test
    void loadsInferredTypes() {
        final Assertion classAssertion = Assertion.createClassAssertion(true);
        final Set<Axiom<URI>> types = new HashSet<>();
        types.add(new AxiomImpl<>(SUBJECT, classAssertion,
                new Value<>(URI.create("http://krizik.felk.cvut.cz/TypeOne"))));
        types.add(new AxiomImpl<>(SUBJECT, classAssertion,
                new Value<>(URI.create("http://krizik.felk.cvut.cz/TypeTwo"))));
        final TypesHandler typesMock = mock(TypesHandler.class);
        when(typesMock.getTypes(SUBJECT, null, true)).thenReturn(types);
        when(adapterMock.getTypesHandler()).thenReturn(typesMock);

        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(classAssertion));
        assertEquals(types.size(), result.size());
        for (Axiom<?> ax : result) {
            assertEquals(classAssertion, ax.getAssertion());
        }
    }

    @Test
    void throwsExceptionWhenReasonerIsNotAvailableForInferredAssertions() {
        final URI opUri = URI.create("http://krizik.felk.cvut.cz/PropertyOne");
        final Assertion op = Assertion.createObjectPropertyAssertion(opUri, true);
        final OntologySnapshot snapshot = new OntologySnapshot(ontology, manager, dataFactory, null);
        final MainAxiomLoader loader = new MainAxiomLoader(adapterMock, snapshot);

        assertThrows(ReasonerNotAvailableException.class, () -> loader.findAxioms(descriptor(op)));
    }

    @Test
    void skipsExplicitAssertionValueIfThereIsTheSameAssertionAlsoWithInference() {
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
        manager.applyChange(
                new AddAxiom(ontology, dataFactory.getOWLObjectPropertyAssertionAxiom(owlOp, individual, commonInd)));

        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(opAsserted, opInferred));
        assertEquals(indSet.size(), result.size());
        for (Axiom<?> ax : result) {
            assertEquals(opInferred, ax.getAssertion());
        }
    }

    @Test
    void loadsStringLiteralValueForExplicitAnnotationPropertyWithCorrectLanguageTag() {
        initExplicitAnnotationPropertyStringsWithLanguageTag();

        final Collection<Axiom<?>> result = sut
                .findAxioms(
                        descriptor(Assertion.createAnnotationPropertyAssertion(URI.create(RDFS_LABEL), LANG, false)));
        checkLoadedAxiomsForStringValue(result, "a");
    }

    private void initExplicitAnnotationPropertyStringsWithLanguageTag() {
        final OWLAnnotationProperty property = dataFactory.getOWLAnnotationProperty(IRI.create(RDFS_LABEL));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLAnnotationAssertionAxiom(individual.getIRI(),
                        dataFactory.getOWLAnnotation(property, dataFactory.getOWLLiteral("a", LANG)))));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLAnnotationAssertionAxiom(individual.getIRI(),
                        dataFactory.getOWLAnnotation(property, dataFactory.getOWLLiteral("b", "cs")))));
    }

    private void checkLoadedAxiomsForStringValue(Collection<Axiom<?>> result, String expected) {
        assertEquals(1, result.size());
        final Axiom<?> ax = result.iterator().next();
        assertEquals(expected, ax.getValue().getValue());
    }

    @Test
    void loadsStringLiteralValueForExplicitDataPropertyWithCorrectLanguageTag() {
        final URI propertyUri = Generator.generateUri();
        initExplicitDataPropertyStringsWithLanguageTag(propertyUri.toString());

        final Collection<Axiom<?>> result = sut
                .findAxioms(descriptor(Assertion.createDataPropertyAssertion(propertyUri, LANG, false)));
        checkLoadedAxiomsForStringValue(result, "a");
    }

    private void initExplicitDataPropertyStringsWithLanguageTag(String propertyUri) {
        final OWLDataProperty property = dataFactory.getOWLDataProperty(IRI.create(propertyUri));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLDataPropertyAssertionAxiom(property, individual, dataFactory.getOWLLiteral("a", LANG))));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLDataPropertyAssertionAxiom(property, individual, dataFactory.getOWLLiteral("b", "cs"))));
    }

    @Test
    void loadsStringLiteralValueForInferredDataPropertyWithCorrectLanguageTag() {
        final OWLDataProperty dp = dataFactory.getOWLDataProperty(IRI.create(RDFS_LABEL));
        final Set<OWLLiteral> values = new HashSet<>();
        values.add(dataFactory.getOWLLiteral("a", LANG));
        values.add(dataFactory.getOWLLiteral("b", "cs"));
        when(reasonerMock.getDataPropertyValues(individual, dp)).thenReturn(values);

        final Collection<Axiom<?>> result = sut
                .findAxioms(descriptor(Assertion.createDataPropertyAssertion(URI.create(RDFS_LABEL), LANG, true)));
        checkLoadedAxiomsForStringValue(result, "a");
    }

    @Test
    void loadsStringLiteralWithCorrectLanguageTagWhenItIsSpecifiedInExplicitDataPropertyAssertion() {
        final String propertyUri = "http://krizik.felk.cvut.cz/dataPropertyOne";
        initExplicitDataPropertyStringsWithLanguageTag(propertyUri);
        final Assertion dpa = Assertion.createDataPropertyAssertion(URI.create(propertyUri), "cs", false);
        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(dpa));
        checkLoadedAxiomsForStringValue(result, "b");
    }

    @Test
    void loadsStringLiteralWithCorrectLanguageTagWhenItIsSpecifiedInExplicitAnnotationPropertyAssertion() {
        initExplicitAnnotationPropertyStringsWithLanguageTag();
        final Assertion apa = Assertion.createAnnotationPropertyAssertion(URI.create(RDFS_LABEL), "cs", false);
        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(apa));
        checkLoadedAxiomsForStringValue(result, "b");
    }

    @Test
    void loadsStringLiteralWithAllLanguagesWhenLanguageTagIsNotSet() {
        initExplicitAnnotationPropertyStringsWithLanguageTag();
        final Assertion apa = Assertion.createAnnotationPropertyAssertion(URI.create(RDFS_LABEL), false);
        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(apa));
        assertEquals(2, result.size());
        final Set<String> values = result.stream().map(ax -> ax.getValue().stringValue()).collect(Collectors.toSet());
        assertTrue(values.contains("a"));
        assertTrue(values.contains("b"));
    }

    @Test
    void loadsStringLiteralWithCorrectLanguageTagWhenSpecifiedOnUnspecifiedDataProperty() {
        final String propertyUri = "http://krizik.felk.cvut.cz/dataPropertyOne";
        initExplicitDataPropertyStringsWithLanguageTag(propertyUri);
        final Assertion assertion = Assertion.createUnspecifiedPropertyAssertion("cs", false);
        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(assertion));
        checkLoadedAxiomsForStringValue(result, "b");
    }

    @Test
    void loadsStringLiteralWithCorrectLanguageTagWhenSpecifiedOnUnspecifiedAnnotationProperty() {
        initExplicitAnnotationPropertyStringsWithLanguageTag();
        final Assertion assertion = Assertion.createUnspecifiedPropertyAssertion("cs", false);
        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(assertion));
        checkLoadedAxiomsForStringValue(result, "b");
    }

    @Test
    void loadsStringLiteralWithCorrectLanguageTagSpecifiedOnInferredDataProperty() {
        final String propertyUri = "http://krizik.felk.cvut.cz/dataPropertyOne";
        final OWLDataProperty dp = dataFactory.getOWLDataProperty(IRI.create(propertyUri));
        final Set<OWLLiteral> values = new HashSet<>();
        values.add(dataFactory.getOWLLiteral("a", LANG));
        values.add(dataFactory.getOWLLiteral("b", "cs"));
        when(reasonerMock.getDataPropertyValues(individual, dp)).thenReturn(values);
        final Assertion assertion = Assertion.createDataPropertyAssertion(URI.create(propertyUri), "cs", true);

        final Collection<Axiom<?>> result = sut.findAxioms(descriptor(assertion));
        checkLoadedAxiomsForStringValue(result, "b");
    }

    @Test
    void loadAxiomsGetsDataPropertyValuesFromImportedOntologiesAsWell() throws Exception {
        final IRI importedIri = IRI.create(Generator.generateUri());
        final OWLOntology imported = manager.createOntology(importedIri);
        manager.applyChange(new AddImport(ontology, dataFactory.getOWLImportsDeclaration(importedIri)));
        final URI dp = Generator.generateUri();
        final int value = 117;
        manager.applyChange(new AddAxiom(imported, dataFactory
                .getOWLDataPropertyAssertionAxiom(dataFactory.getOWLDataProperty(dp.toString()), individual, value)));

        final Collection<Axiom<?>> result =
                sut.findAxioms(descriptor(Assertion.createDataPropertyAssertion(dp, false)));
        assertEquals(1, result.size());
        assertEquals(value, result.iterator().next().getValue().getValue());
    }

    @Test
    void loadAxiomsGetsAnnotationPropertyValuesFromImportedOntologiesAsWell() throws Exception {
        final IRI importedIri = IRI.create(Generator.generateUri());
        final OWLOntology imported = manager.createOntology(importedIri);
        manager.applyChange(new AddImport(ontology, dataFactory.getOWLImportsDeclaration(importedIri)));
        final URI property = Generator.generateUri();
        final int value = 117;
        manager.applyChange(new AddAxiom(imported, dataFactory
                .getOWLAnnotationAssertionAxiom(dataFactory.getOWLAnnotationProperty(property.toString()),
                        individual.getIRI(), dataFactory.getOWLLiteral(value))));

        final Collection<Axiom<?>> result =
                sut.findAxioms(descriptor(Assertion.createAnnotationPropertyAssertion(property, false)));
        assertEquals(1, result.size());
        assertEquals(value, result.iterator().next().getValue().getValue());
    }
}
