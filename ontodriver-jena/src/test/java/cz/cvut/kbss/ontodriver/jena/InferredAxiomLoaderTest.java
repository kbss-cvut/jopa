package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.vocabulary.RDF;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class InferredAxiomLoaderTest extends AxiomLoaderTestBase {

    @Mock
    private InferredStorageConnector connectorMock;

    private AxiomDescriptor descriptor;

    private InferredAxiomLoader axiomLoader;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.descriptor = new AxiomDescriptor(SUBJECT);
        this.axiomLoader = new InferredAxiomLoader(connectorMock);
    }

    @Test
    public void containsChecksForInferredAxiomExistenceInDefaultGraph() {
        final URI clsUri = Generator.generateUri();
        final Axiom<?> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(true),
                new Value<>(NamedResource.create(clsUri)));
        when(connectorMock.containsWithInference(SUBJECT_RES, RDF.type, createResource(clsUri.toString())))
                .thenReturn(true);
        assertTrue(axiomLoader.contains(axiom, null));
        verify(connectorMock).containsWithInference(SUBJECT_RES, RDF.type, createResource(clsUri.toString()));
    }

    @Test
    public void containsChecksForInferredAxiomExistenceInNamedGraph() {
        final URI clsUri = Generator.generateUri();
        final Axiom<?> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(true),
                new Value<>(NamedResource.create(clsUri)));
        when(connectorMock
                .containsWithInference(SUBJECT_RES, RDF.type, createResource(clsUri.toString()), CONTEXT.toString()))
                .thenReturn(true);
        assertTrue(axiomLoader.contains(axiom, CONTEXT));
        verify(connectorMock)
                .containsWithInference(SUBJECT_RES, RDF.type, createResource(clsUri.toString()), CONTEXT.toString());
    }

    @Test
    public void findLoadsValuesOfSpecifiedAssertions() {
        final Assertion aOne = Assertion.createClassAssertion(true);
        final Assertion aTwo = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final Resource cls = createResource(Generator.generateUri().toString());
        final Property property = createProperty(aTwo.getIdentifier().toString());
        final Resource object = createResource(Generator.generateUri().toString());
        when(connectorMock.findWithInference(SUBJECT_RES, RDF.type, null))
                .thenReturn(Collections.singletonList(createStatement(SUBJECT_RES, RDF.type, cls)));
        when(connectorMock.findWithInference(SUBJECT_RES, property, null))
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
        verify(connectorMock).findWithInference(SUBJECT_RES, RDF.type, null);
        verify(connectorMock).findWithInference(SUBJECT_RES, property, null);
    }

    @Test
    public void findLoadsValuesOfAssertionsInContext() {
        final Assertion aOne = Assertion.createClassAssertion(true);
        final Assertion aTwo = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final Resource cls = createResource(Generator.generateUri().toString());
        final Property property = createProperty(aTwo.getIdentifier().toString());
        final Resource object = createResource(Generator.generateUri().toString());
        when(connectorMock.findWithInference(SUBJECT_RES, RDF.type, null, CONTEXT.toString()))
                .thenReturn(Collections.singletonList(createStatement(SUBJECT_RES, RDF.type, cls)));
        when(connectorMock.findWithInference(SUBJECT_RES, property, null, CONTEXT.toString()))
                .thenReturn(Collections.singletonList(createStatement(SUBJECT_RES, property, object)));
        descriptor.addAssertion(aOne);
        descriptor.setAssertionContext(aOne, CONTEXT);
        descriptor.addAssertion(aTwo);
        descriptor.setSubjectContext(CONTEXT);
        final List<Axiom<?>> result = axiomLoader.find(descriptor, mapAssertions(descriptor));
        assertEquals(2, result.size());
        final Optional<Axiom<?>> clsAxiom = result.stream().filter(a -> a.getAssertion().equals(aOne)).findAny();
        assertTrue(clsAxiom.isPresent());
        assertEquals(NamedResource.create(cls.getURI()), clsAxiom.get().getValue().getValue());
        final Optional<Axiom<?>> refAxiom = result.stream().filter(a -> a.getAssertion().equals(aTwo)).findAny();
        assertTrue(refAxiom.isPresent());
        assertEquals(NamedResource.create(object.getURI()), refAxiom.get().getValue().getValue());
        verify(connectorMock).findWithInference(SUBJECT_RES, RDF.type, null, CONTEXT.toString());
        verify(connectorMock).findWithInference(SUBJECT_RES, property, null, CONTEXT.toString());
    }

    @Test
    public void findBySubjectLoadsAllPropertyAxiomsRelatedToSpecifiedSubject() {
        final Assertion aOne = Assertion.createDataPropertyAssertion(Generator.generateUri(), true);
        final Assertion aTwo = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final Literal literal = createTypedLiteral(117);
        final Property propertyOne = createProperty(aOne.getIdentifier().toString());
        final Property propertyTwo = createProperty(aTwo.getIdentifier().toString());
        final Resource object = createResource(Generator.generateUri().toString());
        when(connectorMock.findWithInference(SUBJECT_RES, null, null)).thenReturn(Arrays.asList(
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
        verify(connectorMock).findWithInference(SUBJECT_RES, null, null);
    }

    @Test
    public void findBySubjectSkipsClassAssertions() {
        final Literal literal = createTypedLiteral(117);
        final Property propertyOne = createProperty(Generator.generateUri().toString());
        final Resource object = createResource(Generator.generateUri().toString());
        when(connectorMock.findWithInference(SUBJECT_RES, null, null)).thenReturn(Arrays.asList(
                createStatement(SUBJECT_RES, propertyOne, literal),
                createStatement(SUBJECT_RES, RDF.type, object)
        ));
        final Collection<Axiom<?>> result = axiomLoader.find(SUBJECT, null);
        assertEquals(1, result.size());
        assertFalse(result.stream().anyMatch(a -> a.getAssertion().isClassAssertion()));
    }

    @Test
    public void findBySubjectWorksInContext() {
        final Assertion aOne = Assertion.createDataPropertyAssertion(Generator.generateUri(), true);
        final Assertion aTwo = Assertion.createObjectPropertyAssertion(Generator.generateUri(), true);
        final Literal literal = createTypedLiteral(117);
        final Property propertyOne = createProperty(aOne.getIdentifier().toString());
        final Property propertyTwo = createProperty(aTwo.getIdentifier().toString());
        final Resource object = createResource(Generator.generateUri().toString());
        when(connectorMock.findWithInference(SUBJECT_RES, null, null, CONTEXT.toString())).thenReturn(Arrays.asList(
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
        verify(connectorMock).findWithInference(SUBJECT_RES, null, null, CONTEXT.toString());
    }
}