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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologySnapshot;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver.owlapi.change.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.net.URI;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;
import static org.semanticweb.owlapi.util.OWLAPIStreamUtils.asSet;

@ExtendWith(MockitoExtension.class)
public class AxiomSaverTest {

    private static final NamedResource SUBJECT = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/Individual");
    private static final URI ASSERTION_URI = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/TestAssertion");

    @Mock
    private OwlapiAdapter adapterMock;

    private OWLOntology ontology;
    private OWLOntologyManager manager;
    private OWLDataFactory dataFactory;
    @Mock
    private OWLReasoner reasonerMock;

    private OWLNamedIndividual individual;

    private AxiomValueDescriptor descriptor;

    private AxiomSaver axiomSaver;

    @BeforeEach
    public void setUp() throws Exception {
        final OntologySnapshot snapshot = TestUtils.initRealOntology(reasonerMock);
        this.ontology = spy(snapshot.getOntology());
        this.manager = spy(snapshot.getOntologyManager());
        this.dataFactory = snapshot.getDataFactory();
        final OntologySnapshot snapshotToUse = new OntologySnapshot(ontology, manager, dataFactory, reasonerMock);
        this.axiomSaver = new AxiomSaver(adapterMock, snapshotToUse);
        this.descriptor = new AxiomValueDescriptor(SUBJECT);
        this.individual = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
    }

    @Test
    public void persistValueWithNullDoesNothing() {
        final Assertion dpAssertion = Assertion.createDataPropertyAssertion(ASSERTION_URI, false);
        final Assertion opAssertion = Assertion.createObjectPropertyAssertion(ASSERTION_URI, false);
        final Assertion apAssertion = Assertion.createAnnotationPropertyAssertion(ASSERTION_URI, false);
        descriptor.addAssertionValue(dpAssertion, Value.nullValue());
        descriptor.addAssertionValue(opAssertion, Value.nullValue());
        descriptor.addAssertionValue(apAssertion, Value.nullValue());
        axiomSaver.persist(descriptor);
        verify(manager, never()).applyChanges(anyList());
    }

    @Test
    public void testPersistIndividualWithDataProperty() {
        final Assertion dpAssertion = Assertion.createDataPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-dataAttribute"), false);
        final Set<Object> values = new HashSet<>();
        values.add("StringValue");
        values.add(1111);
        values.add(3.14159D);
        descriptor.addAssertion(dpAssertion);
        values.forEach(value -> descriptor.addAssertionValue(dpAssertion, new Value<>(value)));

        axiomSaver.persist(descriptor);
        verifyDataPropertyValuesInOntology(dpAssertion, values);
        verifyTransactionalChangesRecorded();
    }

    private void verifyTransactionalChangesRecorded() {
        final ArgumentCaptor<List> changesCaptor = ArgumentCaptor.forClass(List.class);
        verify(adapterMock, atLeastOnce()).addTransactionalChanges(changesCaptor.capture());
        final List<?> changes = changesCaptor.getValue();
        for (Object ch : changes) {
            assertTrue(ch instanceof MutableAddAxiom);
        }
    }

    private void verifyDataPropertyValuesInOntology(Assertion assertion, Set<Object> values) {
        final Set<OWLDataPropertyAssertionAxiom> dpAxioms = asSet(ontology.dataPropertyAssertionAxioms(individual));
        assertEquals(values.size(), dpAxioms.size());
        for (OWLDataPropertyAssertionAxiom axiom : dpAxioms) {
            assertEquals(assertion.getIdentifier(), axiom.getProperty().asOWLDataProperty().getIRI().toURI());
            final Object value = OwlapiUtils.owlLiteralToValue(axiom.getObject());
            assertTrue(values.contains(value));
        }
    }

    @Test
    public void testPersistIndividualWithAnnotationProperty() {
        // This just makes sure that the individual explicitly exists in the ontology
        final Assertion apAssertion = Assertion.createAnnotationPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-annotationAttribute"), false);
        final Set<Object> values = new HashSet<>();
        values.add("StringLabel");
        values.add(1111);
        values.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/apValue"));
        descriptor.addAssertion(apAssertion);
        values.forEach(value -> descriptor.addAssertionValue(apAssertion, new Value<>(value)));

        axiomSaver.persist(descriptor);
        verifyAnnotationPropertyValuesInOntology(apAssertion, values);
        verifyTransactionalChangesRecorded();
    }

    private void verifyAnnotationPropertyValuesInOntology(Assertion assertion, Set<Object> values) {
        final Set<OWLAnnotationAssertionAxiom> apAxioms =
                asSet(ontology.annotationAssertionAxioms(individual.getIRI()));
        assertEquals(values.size(), apAxioms.size());
        for (OWLAnnotationAssertionAxiom axiom : apAxioms) {
            assertEquals(assertion.getIdentifier(), axiom.getProperty().getIRI().toURI());
            final OWLAnnotationValue value = axiom.getValue();
            if (value.asIRI().isPresent()) {
                assertTrue(values.contains(value.asIRI().get().toURI()));
            } else {
                final Object v = OwlapiUtils.owlLiteralToValue(value.asLiteral().get());
                assertTrue(values.contains(v));
            }
        }
    }

    @Test
    public void testPersistIndividualWithObjectProperty() {
        final Assertion assertion = Assertion.createObjectPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA"), false);
        descriptor.addAssertion(assertion);
        final Set<NamedResource> values = new HashSet<>();
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA2"));
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA3"));
        values.forEach(value -> descriptor.addAssertionValue(assertion, new Value<>(value)));

        axiomSaver.persist(descriptor);
        verifyObjectPropertyValuesInOntology(assertion, values);
        verifyTransactionalChangesRecorded();
    }

    private void verifyObjectPropertyValuesInOntology(Assertion assertion, Set<NamedResource> values) {
        final Set<OWLObjectPropertyAssertionAxiom> opAxioms = asSet(ontology.objectPropertyAssertionAxioms(individual));
        assertEquals(values.size(), opAxioms.size());
        for (OWLObjectPropertyAssertionAxiom axiom : opAxioms) {
            assertEquals(assertion.getIdentifier(), axiom.getProperty().asOWLObjectProperty().getIRI().toURI());
            final NamedResource target = NamedResource
                    .create(axiom.getObject().asOWLNamedIndividual().getIRI().toURI());
            assertTrue(values.contains(target));
        }
    }

    @Test
    public void testPersistIndividualWithUnspecifiedPropertyTypePropertiesExist() {
        final Assertion opAssertion = Assertion.createPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA"), false);
        descriptor.addAssertion(opAssertion);
        final Set<NamedResource> opValues = new HashSet<>();
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA2"));
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA3"));
        opValues.forEach(value -> descriptor.addAssertionValue(opAssertion, new Value<>(value)));
        final Assertion dpAssertion = Assertion.createPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-dataAttribute"), false);
        final Set<Object> dpValues = new HashSet<>();
        dpValues.add("StringValue");
        descriptor.addAssertion(dpAssertion);
        dpValues.forEach(value -> descriptor.addAssertionValue(dpAssertion, new Value<>(value)));
        // Make sure that the assertions exist in the ontology so that the saver is able to determine the property type
        addDataPropertyAssertion(dpAssertion.getIdentifier());
        addObjectPropertyAssertion(opAssertion.getIdentifier());

        axiomSaver.persist(descriptor);
        verifyDataPropertyValuesInOntology(dpAssertion, dpValues);
        verifyObjectPropertyValuesInOntology(opAssertion, opValues);
        verifyTransactionalChangesRecorded();
    }

    private void addDataPropertyAssertion(URI property) {
        final OWLDataProperty dataProperty = dataFactory.getOWLDataProperty(IRI.create(property));
        final OWLIndividual ind = dataFactory.getOWLAnonymousIndividual();
        final AddAxiom ax = new AddAxiom(ontology,
                dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, ind, dataFactory.getOWLLiteral(true)));
        manager.applyChange(ax);
    }

    private void addObjectPropertyAssertion(URI property) {
        final OWLObjectProperty objectProperty = dataFactory.getOWLObjectProperty(IRI.create(property));
        final OWLIndividual subject = dataFactory.getOWLAnonymousIndividual();
        final OWLIndividual target = dataFactory.getOWLAnonymousIndividual();
        final AddAxiom ax = new AddAxiom(ontology,
                dataFactory.getOWLObjectPropertyAssertionAxiom(objectProperty, subject, target));
        manager.applyChange(ax);
    }

    @Test
    public void testPersistIndividualWithUnspecifiedPropertyTypePropertiesUnknown() {
        final Assertion opAssertion = Assertion.createPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA"), false);
        descriptor.addAssertion(opAssertion);
        final Set<NamedResource> opValues = new HashSet<>();
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA2"));
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA3"));
        opValues.add(NamedResource.create("non-uri-value"));
        opValues.forEach(value -> descriptor.addAssertionValue(opAssertion, new Value<>(value)));

        axiomSaver.persist(descriptor);
        verifyObjectPropertyValuesInOntology(opAssertion, opValues);
        verifyTransactionalChangesRecorded();
    }
}
