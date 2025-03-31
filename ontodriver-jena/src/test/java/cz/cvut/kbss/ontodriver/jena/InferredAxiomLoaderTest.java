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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class InferredAxiomLoaderTest extends AxiomLoaderTestBase {

    @Mock
    private InferredStorageConnector connectorMock;

    private AxiomDescriptor descriptor;

    private InferredAxiomLoader axiomLoader;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        this.descriptor = new AxiomDescriptor(SUBJECT);
        this.axiomLoader = new InferredAxiomLoader(connectorMock);
    }

    @Test
    void containsChecksForInferredAxiomExistenceInDefaultGraph() {
        final URI clsUri = Generator.generateUri();
        final Axiom<?> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(true),
                new Value<>(NamedResource.create(clsUri)));
        when(connectorMock.containsWithInference(SUBJECT_RES, RDF.type, createResource(clsUri.toString()), Collections.emptySet()))
                .thenReturn(true);
        assertTrue(axiomLoader.contains(axiom, Collections.emptySet()));
        verify(connectorMock).containsWithInference(SUBJECT_RES, RDF.type, createResource(clsUri.toString()), Collections.emptySet());
    }

    @Test
    void containsChecksForInferredAxiomExistenceInNamedGraph() {
        final URI clsUri = Generator.generateUri();
        final Axiom<?> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(true),
                new Value<>(NamedResource.create(clsUri)));
        when(connectorMock
                .containsWithInference(SUBJECT_RES, RDF.type, createResource(clsUri.toString()),
                        Collections.singleton(CONTEXT.toString())))
                .thenReturn(true);
        assertTrue(axiomLoader.contains(axiom, Collections.singleton(CONTEXT)));
        verify(connectorMock)
                .containsWithInference(SUBJECT_RES, RDF.type, createResource(clsUri.toString()),
                        Collections.singleton(CONTEXT.toString()));
    }

    @Test
    void findLoadsValuesOfSpecifiedAssertions() {
        final Assertion aOne = Assertion.createClassAssertion(true);
        final Assertion aTwo = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final Resource cls = createResource(Generator.generateUri().toString());
        final Property property = createProperty(aTwo.getIdentifier().toString());
        final Resource object = createResource(Generator.generateUri().toString());
        when(connectorMock.findWithInference(SUBJECT_RES, RDF.type, null, Collections.emptySet()))
                .thenReturn(Collections.singletonList(createStatement(SUBJECT_RES, RDF.type, cls)));
        when(connectorMock.findWithInference(SUBJECT_RES, property, null, Collections.emptySet()))
                .thenReturn(Collections.singletonList(createStatement(SUBJECT_RES, property, object)));
        descriptor.addAssertion(aOne);
        descriptor.addAssertion(aTwo);
        final List<Axiom<?>> result = axiomLoader.find(descriptor, mapAssertions(descriptor));
        assertEquals(2, result.size());
        final Optional<Axiom<?>> clsAxiom = result.stream().filter(a -> a.getAssertion().equals(aOne)).findAny();
        assertTrue(clsAxiom.isPresent());
        assertEquals(NamedResource.create(cls.getURI()), clsAxiom.get().getValue().getValue());
        final Optional<Axiom<?>> refAxiom = result.stream().filter(a -> a.getAssertion().equals(aTwo)).findAny();
        assertTrue(refAxiom.isPresent());
        assertEquals(NamedResource.create(object.getURI()), refAxiom.get().getValue().getValue());
        verify(connectorMock).findWithInference(SUBJECT_RES, RDF.type, null, Collections.emptySet());
        verify(connectorMock).findWithInference(SUBJECT_RES, property, null, Collections.emptySet());
    }

    @Test
    void findLoadsValuesOfAssertionsInContext() {
        final Assertion aOne = Assertion.createClassAssertion(true);
        final Assertion aTwo = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final Resource cls = createResource(Generator.generateUri().toString());
        final Property property = createProperty(aTwo.getIdentifier().toString());
        final Resource object = createResource(Generator.generateUri().toString());
        when(connectorMock.findWithInference(SUBJECT_RES, RDF.type, null, Collections.singleton(CONTEXT.toString())))
                .thenReturn(Collections.singletonList(createStatement(SUBJECT_RES, RDF.type, cls)));
        when(connectorMock.findWithInference(SUBJECT_RES, property, null, Collections.singleton(CONTEXT.toString())))
                .thenReturn(Collections.singletonList(createStatement(SUBJECT_RES, property, object)));
        descriptor.addAssertion(aOne);
        descriptor.addAssertionContext(aOne, CONTEXT);
        descriptor.addAssertion(aTwo);
        descriptor.addSubjectContext(CONTEXT);
        final List<Axiom<?>> result = axiomLoader.find(descriptor, mapAssertions(descriptor));
        assertEquals(2, result.size());
        final Optional<Axiom<?>> clsAxiom = result.stream().filter(a -> a.getAssertion().equals(aOne)).findAny();
        assertTrue(clsAxiom.isPresent());
        assertEquals(NamedResource.create(cls.getURI()), clsAxiom.get().getValue().getValue());
        final Optional<Axiom<?>> refAxiom = result.stream().filter(a -> a.getAssertion().equals(aTwo)).findAny();
        assertTrue(refAxiom.isPresent());
        assertEquals(NamedResource.create(object.getURI()), refAxiom.get().getValue().getValue());
        verify(connectorMock).findWithInference(SUBJECT_RES, RDF.type, null, Collections.singleton(CONTEXT.toString()));
        verify(connectorMock).findWithInference(SUBJECT_RES, property, null, Collections.singleton(CONTEXT.toString()));
    }

    @Test
    void findBySubjectLoadsAllPropertyAxiomsRelatedToSpecifiedSubject() {
        final Assertion aOne = Assertion.createDataPropertyAssertion(Generator.generateUri(), true);
        final Assertion aTwo = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final Literal literal = createTypedLiteral(117);
        final Property propertyOne = createProperty(aOne.getIdentifier().toString());
        final Property propertyTwo = createProperty(aTwo.getIdentifier().toString());
        final Resource object = createResource(Generator.generateUri().toString());
        when(connectorMock.findWithInference(SUBJECT_RES, null, null, Collections.emptySet())).thenReturn(Arrays.asList(
                createStatement(SUBJECT_RES, propertyOne, literal),
                createStatement(SUBJECT_RES, propertyTwo, object)
        ));
        final Collection<Axiom<?>> result = axiomLoader.find(SUBJECT, null);
        assertEquals(2, result.size());
        final Optional<Axiom<?>> dpAxiom = result.stream().filter(a -> a.getAssertion().equals(aOne)).findAny();
        assertTrue(dpAxiom.isPresent());
        assertEquals(117, dpAxiom.get().getValue().getValue());
        final Optional<Axiom<?>> refAxiom = result.stream().filter(a -> a.getAssertion().equals(aTwo)).findAny();
        assertTrue(refAxiom.isPresent());
        assertEquals(NamedResource.create(object.getURI()), refAxiom.get().getValue().getValue());
        verify(connectorMock).findWithInference(SUBJECT_RES, null, null, Collections.emptySet());
    }

    @Test
    void findBySubjectSkipsClassAssertions() {
        final Literal literal = createTypedLiteral(117);
        final Property propertyOne = createProperty(Generator.generateUri().toString());
        final Resource object = createResource(Generator.generateUri().toString());
        when(connectorMock.findWithInference(SUBJECT_RES, null, null, Collections.emptySet())).thenReturn(Arrays.asList(
                createStatement(SUBJECT_RES, propertyOne, literal),
                createStatement(SUBJECT_RES, RDF.type, object)
        ));
        final Collection<Axiom<?>> result = axiomLoader.find(SUBJECT, null);
        assertEquals(1, result.size());
        assertFalse(result.stream().anyMatch(a -> a.getAssertion().isClassAssertion()));
    }

    @Test
    void findBySubjectWorksInContext() {
        final Assertion aOne = Assertion.createDataPropertyAssertion(Generator.generateUri(), true);
        final Assertion aTwo = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final Literal literal = createTypedLiteral(117);
        final Property propertyOne = createProperty(aOne.getIdentifier().toString());
        final Property propertyTwo = createProperty(aTwo.getIdentifier().toString());
        final Resource object = createResource(Generator.generateUri().toString());
        when(connectorMock.findWithInference(SUBJECT_RES, null, null, Collections.singleton(CONTEXT.toString())))
                .thenReturn(Arrays.asList(
                        createStatement(SUBJECT_RES, propertyOne, literal),
                        createStatement(SUBJECT_RES, propertyTwo, object)
                ));
        final Collection<Axiom<?>> result = axiomLoader.find(SUBJECT, CONTEXT);
        assertEquals(2, result.size());
        final Optional<Axiom<?>> dpAxiom = result.stream().filter(a -> a.getAssertion().equals(aOne)).findAny();
        assertTrue(dpAxiom.isPresent());
        assertEquals(117, dpAxiom.get().getValue().getValue());
        final Optional<Axiom<?>> refAxiom = result.stream().filter(a -> a.getAssertion().equals(aTwo)).findAny();
        assertTrue(refAxiom.isPresent());
        assertEquals(NamedResource.create(object.getURI()), refAxiom.get().getValue().getValue());
        verify(connectorMock).findWithInference(SUBJECT_RES, null, null, Collections.singleton(CONTEXT.toString()));
    }
}
