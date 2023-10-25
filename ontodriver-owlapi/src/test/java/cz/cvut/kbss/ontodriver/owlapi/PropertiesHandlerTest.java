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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.config.Constants;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.Generator;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;
import org.semanticweb.owlapi.vocab.OWL2Datatype;

import java.net.URI;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class PropertiesHandlerTest {

    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#Entity");
    private static final NamedResource INDIVIDUAL = NamedResource.create(PK);

    private static final URI DP_ONE = URI.create("http://krizik.felk.cvut.cz/dpOne");
    private static final URI DP_TWO = URI.create("http://krizik.felk.cvut.cz/dpTwo");
    private static final URI OP_ONE = URI.create("http://krizik.felk.cvut.cz/opOne");

    @Mock
    private OwlapiAdapter adapterMock;


    private OWLOntology ontology;


    private OWLOntologyManager manager;
    @Mock
    private OWLReasoner reasonerMock;

    private OWLDataFactory dataFactory;

    private PropertiesHandler propertiesHandler;


    private final Set<Object> dataValues = new HashSet<>();
    private final Set<Object> objectValues = new HashSet<>();

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        final OntologySnapshot snapshot = TestUtils.initRealOntology(reasonerMock);
        this.ontology = snapshot.getOntology();
        this.manager = snapshot.getOntologyManager();
        this.dataFactory = snapshot.getDataFactory();
        this.propertiesHandler = new PropertiesHandler(adapterMock, snapshot);
    }

    @Test
    public void getPropertiesLoadsExplicitPropertiesFromOntology() {
        initSampleProperties();
        final Collection<Axiom<?>> axioms = propertiesHandler.getProperties(INDIVIDUAL, false);
        assertFalse(axioms.isEmpty());
        verifyAxioms(axioms, Assertion.AssertionType.DATA_PROPERTY, dataValues);
        verifyAxioms(axioms, Assertion.AssertionType.OBJECT_PROPERTY, objectValues);
    }

    private void initSampleProperties() {
        initSampleDataProperties();
        initSampleObjectProperties();
    }

    private void initSampleDataProperties() {
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(PK));
        final OWLDataProperty dpOne = dataFactory.getOWLDataProperty(IRI.create(DP_ONE));
        dataValues.add(100);
        dataValues.add(1000);
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLDataPropertyAssertionAxiom(dpOne, individual, dataFactory.getOWLLiteral("100", OWL2Datatype.XSD_INT))));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLDataPropertyAssertionAxiom(dpOne, individual, dataFactory.getOWLLiteral("1000", OWL2Datatype.XSD_INT))));
        final OWLDataProperty dpTwo = dataFactory.getOWLDataProperty(IRI.create(DP_TWO));
        dataValues.add("Test");
        dataValues.add("TestTwo");
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLDataPropertyAssertionAxiom(dpTwo, individual, "Test")));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLDataPropertyAssertionAxiom(dpTwo, individual, "TestTwo")));
    }

    private void initSampleObjectProperties() {
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(PK));
        final OWLObjectProperty op = dataFactory.getOWLObjectProperty(IRI.create(OP_ONE));
        objectValues.add(INDIVIDUAL);
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLObjectPropertyAssertionAxiom(op, individual, individual)));
    }

    private void verifyAxioms(Collection<Axiom<?>> axioms, Assertion.AssertionType assertionType,
                              Set<?> expectedValues) {
        boolean found;
        for (Object val : expectedValues) {
            found = false;
            for (Axiom<?> ax : axioms) {
                if (ax.getValue().getValue().equals(val)) {
                    found = true;
                    assertEquals(assertionType, ax.getAssertion().getType());
                    break;
                }
            }
            assertTrue(found);
        }
    }

    @Test
    public void getPropertiesReturnsEmptyCollectionsForNonExistingIndividual() {
        final Collection<Axiom<?>> axioms = propertiesHandler.getProperties(INDIVIDUAL, false);
        assertNotNull(axioms);
        assertTrue(axioms.isEmpty());
    }

    @Test
    public void getPropertiesWithInferenceGetsInferredProperties() {
        final OWLNamedIndividual individual = dataFactory.getOWLNamedIndividual(IRI.create(PK));
        manager.applyChange(new AddAxiom(ontology,
                dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLDataProperty(IRI.create(DP_ONE)))));
        manager.applyChange(new AddAxiom(ontology,
                dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLDataProperty(IRI.create(DP_TWO)))));
        manager.applyChange(new AddAxiom(ontology,
                dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLObjectProperty(IRI.create(OP_ONE)))));
        when(reasonerMock.getDataPropertyValues(eq(individual), any(OWLDataProperty.class)))
                .thenReturn(Collections.emptySet());
        when(reasonerMock.getObjectPropertyValues(eq(individual),
                any(OWLObjectProperty.class))).thenReturn(new OWLNamedIndividualNodeSet());

        final Collection<Axiom<?>> axioms = propertiesHandler.getProperties(INDIVIDUAL, true);

        assertNotNull(axioms);
        verify(reasonerMock, atLeastOnce()).getDataPropertyValues(eq(individual), any(OWLDataProperty.class));
        verify(reasonerMock, atLeastOnce()).getObjectPropertyValues(eq(individual), any(OWLObjectProperty.class));
    }

    @Test
    public void addPropertiesForExistingDataPropertyAddsAssertionsToOntology() {
        final Assertion assertion = Assertion.createDataPropertyAssertion(DP_ONE, false);
        final Set<Value<?>> values = new HashSet<>(
                Arrays.asList(new Value<>("One"), new Value<>("Two"), new Value<>("Three")));
        manager.applyChange(new AddAxiom(ontology,
                dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLDataProperty(IRI.create(DP_ONE)))));

        propertiesHandler.addProperties(INDIVIDUAL, Collections.singletonMap(assertion, values));
        verifyAddedDataPropertyAxioms(values);
    }

    @Test
    public void addPropertiesForExistingDataPropertyAddsValuesOfCorrespondingTypes() {
        final Assertion assertion = Assertion.createDataPropertyAssertion(DP_ONE, false);
        final Set<Object> baseValues = new HashSet<>(Arrays.asList(false, 1, new Date()));
        final Set<Value<?>> values = baseValues.stream().map(Value::new).collect(Collectors.toSet());
        manager.applyChange(new AddAxiom(ontology,
                dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLDataProperty(IRI.create(DP_ONE)))));

        propertiesHandler.addProperties(INDIVIDUAL, Collections.singletonMap(assertion, values));
        verifyAddedDataPropertyAxioms(values);
    }

    private void verifyAddedDataPropertyAxioms(Collection<Value<?>> expectedValues) {
        expectedValues.forEach(v -> ontology.containsAxiom(dataFactory.getOWLDataPropertyAssertionAxiom(
                dataFactory.getOWLDataProperty(IRI.create(DP_ONE)),
                dataFactory.getOWLNamedIndividual(IRI.create(INDIVIDUAL.getIdentifier())),
                OwlapiUtils.createOWLLiteralFromValue(v.getValue(), Constants.DEFAULT_LANGUAGE))));
    }

    @Test
    public void addPropertiesForExistingObjectPropertyAddsAssertionsToOntology() {
        final Assertion assertion = Assertion.createObjectPropertyAssertion(OP_ONE, false);
        final Set<Value<?>> values = new HashSet<>(Arrays.asList(
                new Value<>(NamedResource.create("http://individualOne")),
                new Value<>(NamedResource.create("http://individualTwo"))));
        manager.applyChange(new AddAxiom(ontology,
                dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLObjectProperty(IRI.create(OP_ONE)))));

        propertiesHandler.addProperties(INDIVIDUAL, Collections.singletonMap(assertion, values));
        verifyAddedObjectPropertyAxioms(values);
    }

    private void verifyAddedObjectPropertyAxioms(Collection<Value<?>> expectedValues) {
        final Set<String> directValues = expectedValues.stream().map(Value::stringValue).collect(Collectors.toSet());
        ontology.objectPropertyAssertionAxioms(dataFactory.getOWLNamedIndividual(PK.toString())).forEach(
                ax -> assertTrue(directValues.contains(ax.getObject().asOWLNamedIndividual().getIRI().toString())));
    }

    @Test
    public void addPropertiesForExistingObjectPropertyHandlesIndividualsAsStringAndUri() {
        final Assertion assertion = Assertion.createObjectPropertyAssertion(OP_ONE, false);
        final Set<Value<?>> values = new HashSet<>(Arrays.asList(
                new Value<>("http://individualOne"),
                new Value<>(URI.create("http://individualTwo"))));
        manager.applyChange(new AddAxiom(ontology,
                dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLDataProperty(IRI.create(DP_ONE)))));

        propertiesHandler.addProperties(INDIVIDUAL, Collections.singletonMap(assertion, values));
        verifyAddedObjectPropertyAxioms(values);
    }

    @Test
    public void addPropertiesInfersDataPropertyAndLiteralsFromValuesForUnknownProperty() {
        final Assertion assertion = Assertion.createPropertyAssertion(DP_ONE, false);
        final Set<Object> typedValues = new HashSet<>(Arrays.asList(true, 117L, new Date()));
        final Set<Value<?>> values = typedValues.stream().map(Value::new).collect(Collectors.toSet());

        propertiesHandler.addProperties(INDIVIDUAL, Collections.singletonMap(assertion, values));
        manager.applyChange(new AddAxiom(ontology,
                dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLDataProperty(IRI.create(DP_ONE)))));
    }

    @Test
    public void addPropertiesInfersObjectPropertyAndIrisFromValuesForUnknownProperty() throws Exception {
        final Assertion assertion = Assertion.createPropertyAssertion(OP_ONE, false);
        final Set<Object> typedValues = new HashSet<>(
                Arrays.asList("http://individualOne", URI.create("http://individualTwo"),
                        new URL("http://individualThree")));
        final Set<Value<?>> values = typedValues.stream().map(Value::new).collect(Collectors.toSet());

        propertiesHandler.addProperties(INDIVIDUAL, Collections.singletonMap(assertion, values));
        verifyAddedObjectPropertyAxioms(values);
    }

    @Test
    public void addPropertiesAddsValuesOfKnownAnnotationProperty() {
        final Assertion assertion = Assertion.createPropertyAssertion(OP_ONE, false);
        final Set<Object> individualValues = new HashSet<>(Collections.singletonList("http://individualOne"));
        final Set<Object> literalValues = new HashSet<>(Arrays.asList("label", 1));
        final Set<Value<?>> values = literalValues.stream().map(Value::new).collect(Collectors.toSet());
        values.addAll(individualValues.stream().map(Value::new).collect(Collectors.toSet()));
        manager.applyChange(new AddAxiom(ontology,
                dataFactory.getOWLDeclarationAxiom(dataFactory.getOWLAnnotationProperty(IRI.create(OP_ONE)))));

        propertiesHandler.addProperties(INDIVIDUAL, Collections.singletonMap(assertion, values));
        verifyAddedAnnotationPropertyAxioms(values);
    }

    private void verifyAddedAnnotationPropertyAxioms(Collection<Value<?>> expectedValues) {
        final Set<Object> directValues = expectedValues.stream().map(Value::getValue).collect(Collectors.toSet());
        ontology.annotationAssertionAxioms(IRI.create(PK.toString())).forEach(
                ax -> assertTrue(directValues
                        .contains(ax.getAnnotation().annotationValue().isLiteral() ?
                                OwlapiUtils.owlLiteralToValue(ax.getAnnotation().annotationValue().asLiteral().get()) :
                                ax.getAnnotation().annotationValue().asIRI().get().toString())));
    }


    @Test
    public void removePropertiesRemovesSpecifiedPropertyValues() {
        final Assertion assertion = Assertion.createPropertyAssertion(DP_ONE, false);
        final String value = "Test";
        final Map<Assertion, Set<Value<?>>> toRemove = Collections
                .singletonMap(assertion, Collections.singleton(new Value<>(value)));
        manager.applyChange(new AddAxiom(ontology, dataFactory
                .getOWLDataPropertyAssertionAxiom(dataFactory.getOWLDataProperty(DP_ONE.toString()),
                        dataFactory.getOWLNamedIndividual(PK.toString()), value)));


        propertiesHandler.removeProperties(INDIVIDUAL, toRemove);
        assertTrue(ontology.dataPropertyAssertionAxioms(dataFactory.getOWLNamedIndividual(PK.toString()))
                .collect(Collectors.toSet()).isEmpty());
    }

    @Test
    void loadAxiomsGetsObjectPropertyValuesFromImportedOntologiesAsWell() throws Exception {
        final IRI importedIri = IRI.create(Generator.generateUri());
        final OWLOntology imported = manager.createOntology(importedIri);
        manager.applyChange(new AddImport(ontology, dataFactory.getOWLImportsDeclaration(importedIri)));
        final URI property = Generator.generateUri();
        final OWLNamedIndividual object = dataFactory.getOWLNamedIndividual(Generator.generateUri().toString());
        manager.applyChange(new AddAxiom(imported, dataFactory
                .getOWLObjectPropertyAssertionAxiom(dataFactory.getOWLObjectProperty(property.toString()),
                        dataFactory.getOWLNamedIndividual(PK.toString()), object)));

        final Collection<Axiom<?>> result = propertiesHandler.getProperties(INDIVIDUAL, false);
        assertEquals(1, result.size());
        assertEquals(NamedResource.create(object.getIRI().toURI()), result.iterator().next().getValue().getValue());
    }
}
