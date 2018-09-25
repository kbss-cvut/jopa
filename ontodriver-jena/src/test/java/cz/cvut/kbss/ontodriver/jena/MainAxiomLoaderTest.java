package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.connector.InferredStorageConnector;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.*;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.vocabulary.RDF;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

public class MainAxiomLoaderTest {

    private static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    private static final Resource SUBJECT_RES = createResource(SUBJECT.getIdentifier().toString());
    private static final Property PROPERTY = createProperty(Generator.generateUri().toString());
    private static final NamedResource OBJECT = NamedResource.create(Generator.generateUri());
    private static final Resource OBJECT_RES = createResource(OBJECT.getIdentifier().toString());

    @Mock
    private StorageConnector connectorMock;

    @Mock
    private InferredStorageConnector inferredConnectorMock;

    private MainAxiomLoader axiomLoader;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.axiomLoader = new MainAxiomLoader(connectorMock, inferredConnectorMock, null);
    }

    @Test
    public void containsChecksForNonInferredStatementsOnlyWhenAssertionIsNotInferred() {
        final Resource cls = createResource(Generator.generateUri().toString());
        when(connectorMock.contains(SUBJECT_RES, RDF.type, cls, null)).thenReturn(true);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(cls.getURI())));
        assertTrue(axiomLoader.contains(axiom, null));
        verify(connectorMock).contains(SUBJECT_RES, RDF.type, cls, null);
        verify(inferredConnectorMock, never()).containsWithInference(SUBJECT_RES, RDF.type, cls, null);
    }

    @Test
    public void containsChecksForInferredStatementsWhenAssertionIsInferred() {
        final Resource cls = createResource(Generator.generateUri().toString());
        when(inferredConnectorMock.containsWithInference(SUBJECT_RES, RDF.type, cls, null)).thenReturn(true);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(true),
                new Value<>(NamedResource.create(cls.getURI())));
        assertTrue(axiomLoader.contains(axiom, null));
        verify(connectorMock, never()).contains(SUBJECT_RES, RDF.type, cls, null);
        verify(inferredConnectorMock).containsWithInference(SUBJECT_RES, RDF.type, cls, null);
    }

    @Test
    public void findBySubjectUsesNonInferredConnector() {
        final Axiom<NamedResource> axiom =
                new AxiomImpl<>(SUBJECT, Assertion.createObjectPropertyAssertion(URI.create(PROPERTY.getURI()), false),
                        new Value<>(OBJECT));
        when(connectorMock.find(SUBJECT_RES, null, null, null)).thenReturn(Collections.singletonList(
                createStatement(SUBJECT_RES, PROPERTY, OBJECT_RES)
        ));
        final Collection<Axiom<?>> result = axiomLoader.find(SUBJECT, null);
        assertEquals(1, result.size());
        assertTrue(result.contains(axiom));
    }

    @Test
    public void findWithInferenceUsesInferredConnector() {
        final Axiom<NamedResource> axiom =
                new AxiomImpl<>(SUBJECT, Assertion.createObjectPropertyAssertion(URI.create(PROPERTY.getURI()), true),
                        new Value<>(OBJECT));
        when(inferredConnectorMock.findWithInference(SUBJECT_RES, null, null, null))
                .thenReturn(Collections.singletonList(
                        createStatement(SUBJECT_RES, PROPERTY, OBJECT_RES)
                ));
        final Collection<Axiom<?>> result = axiomLoader.findWithInference(SUBJECT, null);
        assertEquals(1, result.size());
        assertTrue(result.contains(axiom));
    }

    @Test
    public void findUsesExplicitLoaderForNonInferredAssertionsAndInferredLoaderForInferredAssertions() {
        final Axiom<NamedResource> inferred =
                new AxiomImpl<>(SUBJECT, Assertion.createObjectPropertyAssertion(URI.create(PROPERTY.getURI()), true),
                        new Value<>(OBJECT));
        when(inferredConnectorMock.findWithInference(SUBJECT_RES, PROPERTY, null, null))
                .thenReturn(Collections.singletonList(
                        createStatement(SUBJECT_RES, PROPERTY, OBJECT_RES)
                ));
        final Property proTwo = createProperty(Generator.generateUri().toString());
        final Axiom<Integer> asserted =
                new AxiomImpl<>(SUBJECT, Assertion.createDataPropertyAssertion(URI.create(proTwo.getURI()), false),
                        new Value<>(117));
        when(connectorMock.find(SUBJECT_RES, null, null, null)).thenReturn(Collections.singletonList(
                createStatement(SUBJECT_RES, proTwo, createTypedLiteral(117))
        ));
        final AxiomDescriptor descriptor = new AxiomDescriptor(SUBJECT);
        descriptor.addAssertion(inferred.getAssertion());
        descriptor.addAssertion(asserted.getAssertion());
        final Collection<Axiom<?>> result = axiomLoader.find(descriptor);
        assertEquals(2, result.size());
        assertTrue(result.contains(asserted));
        assertTrue(result.contains(inferred));
    }
}