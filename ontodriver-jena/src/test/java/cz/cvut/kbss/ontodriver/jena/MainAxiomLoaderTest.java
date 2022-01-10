/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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
        MockitoAnnotations.openMocks(this);
        this.axiomLoader = new MainAxiomLoader(connectorMock, inferredConnectorMock);
    }

    @Test
    public void containsChecksForNonInferredStatementsOnlyWhenAssertionIsNotInferred() {
        final Resource cls = createResource(Generator.generateUri().toString());
        when(connectorMock.contains(SUBJECT_RES, RDF.type, cls, Collections.emptySet())).thenReturn(true);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(cls.getURI())));
        assertTrue(axiomLoader.contains(axiom, Collections.emptySet()));
        verify(connectorMock).contains(SUBJECT_RES, RDF.type, cls, Collections.emptySet());
        verify(inferredConnectorMock, never()).containsWithInference(SUBJECT_RES, RDF.type, cls, Collections.emptySet());
    }

    @Test
    public void containsChecksForInferredStatementsWhenAssertionIsInferred() {
        final Resource cls = createResource(Generator.generateUri().toString());
        when(inferredConnectorMock.containsWithInference(SUBJECT_RES, RDF.type, cls, Collections.emptySet())).thenReturn(true);
        final Axiom<NamedResource> axiom = new AxiomImpl<>(SUBJECT, Assertion.createClassAssertion(true),
                new Value<>(NamedResource.create(cls.getURI())));
        assertTrue(axiomLoader.contains(axiom, Collections.emptySet()));
        verify(connectorMock, never()).contains(SUBJECT_RES, RDF.type, cls, Collections.emptySet());
        verify(inferredConnectorMock).containsWithInference(SUBJECT_RES, RDF.type, cls, Collections.emptySet());
    }

    @Test
    public void findBySubjectUsesNonInferredConnector() {
        final Axiom<NamedResource> axiom =
                new AxiomImpl<>(SUBJECT, Assertion.createObjectPropertyAssertion(URI.create(PROPERTY.getURI()), false),
                        new Value<>(OBJECT));
        when(connectorMock.find(SUBJECT_RES, null, null, Collections.emptySet())).thenReturn(Collections.singletonList(
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
        when(inferredConnectorMock.findWithInference(SUBJECT_RES, null, null, Collections.emptySet()))
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
        when(inferredConnectorMock.findWithInference(SUBJECT_RES, PROPERTY, null, Collections.emptySet()))
                .thenReturn(Collections.singletonList(
                        createStatement(SUBJECT_RES, PROPERTY, OBJECT_RES)
                ));
        final Property proTwo = createProperty(Generator.generateUri().toString());
        final Axiom<Integer> asserted =
                new AxiomImpl<>(SUBJECT, Assertion.createDataPropertyAssertion(URI.create(proTwo.getURI()), false),
                        new Value<>(117));
        when(connectorMock.find(SUBJECT_RES, null, null, Collections.emptySet())).thenReturn(Collections.singletonList(
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
