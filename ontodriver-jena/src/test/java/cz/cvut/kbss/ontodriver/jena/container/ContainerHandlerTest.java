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
package cz.cvut.kbss.ontodriver.jena.container;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.jena.JenaDataSource;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.jena.connector.SnapshotConnectorFactory;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ContainerHandlerTest {

    private final NamedResource owner = NamedResource.create(Generator.generateUri());

    private ConnectorFactory connectorFactory;

    private StorageConnector connector;

    private Dataset sharedDataset;

    private ContainerHandler sut;

    @BeforeEach
    void setUp() {
        final DriverConfiguration config = new DriverConfiguration(OntologyStorageProperties.driver(JenaDataSource.class.getName())
                                                                                            .physicalUri("mem:test")
                                                                                            .build());
        config.setProperty(JenaConfigParam.STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        this.connectorFactory = new SnapshotConnectorFactory(config);
        this.connector = connectorFactory.createConnector();
        this.sut = new ContainerHandler(connector);
    }

    @Test
    void readContainerReturnsEmptyListWhenContainerDoesNotExist() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);

        connector.begin();
        final List<Axiom<?>> result = sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void readContainerReturnsListOfAxiomsRepresentingContainerContent() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "3"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        loadData(ttl);

        connector.begin();
        final List<Axiom<?>> result = sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertEquals(3, result.size());
        for (int i = 0; i < result.size(); i++) {
            assertEquals(owner, result.get(i).getSubject());
            assertEquals(property, result.get(i).getAssertion());
            assertEquals(new Value<>(i + 1), result.get(i).getValue());
        }
    }

    private void loadData(String ttl) {
        this.sharedDataset = DatasetFactory.createTxnMem();
        sharedDataset.begin(ReadWrite.WRITE);
        sharedDataset.getDefaultModel()
                     .read(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE.getLang().getLabel());
        sharedDataset.commit();
        connectorFactory.setDataset(sharedDataset);
    }

    @Test
    void readContainerReturnsListOfAxiomsWithDuplicateValuesRepresentingContainerContentWithDuplicates() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "1"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        loadData(ttl);

        connector.begin();
        final List<Axiom<?>> result = sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertEquals(List.of(1, 2, 1), result.stream().map(a -> a.getValue().getValue()).toList());
    }

    @Test
    void readContainerReturnsListOfAxiomsRepresentingContainerContentFromContext() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final URI context = Generator.generateUri();
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        this.sharedDataset = DatasetFactory.createTxnMem();
        sharedDataset.begin(ReadWrite.WRITE);
        sharedDataset.getNamedModel(context.toString())
                     .read(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE.getLang().getLabel());
        sharedDataset.commit();
        connectorFactory.setDataset(sharedDataset);

        connector.begin();
        final List<Axiom<?>> result = sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property, context));
        assertNotNull(result);
        assertEquals(2, result.size());
        for (int i = 0; i < result.size(); i++) {
            assertEquals(owner, result.get(i).getSubject());
            assertEquals(property, result.get(i).getAssertion());
            assertEquals(new Value<>(i + 1), result.get(i).getValue());
        }
    }

    @Test
    void readContainerSupportsContainerRepresentedByBlankNode() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> _:xxx .
                _:xxx rdf:_1 "1"^^xsd:int .
                _:xxx rdf:_2 "2"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        loadData(ttl);

        connector.begin();
        final List<Axiom<?>> result = sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void readContainerThrowsIntegrityConstraintViolatedExceptionWhenContainerValueIsNotUnique() {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "2"^^xsd:int .
                <%s> <%s> <https://example.com/hasIsolationLevels/anotherObject> .
                """.formatted(owner.toString(), property.getIdentifier(), owner.toString(), property.getIdentifier());
        loadData(ttl);

        connector.begin();
        assertThrows(IntegrityConstraintViolatedException.class, () -> sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property)));
    }

    @Test
    void readContainerPreservesOrderBasedOnContainerMembershipPropertiesNumbering() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "3"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        loadData(ttl);

        connector.begin();
        final List<Axiom<?>> result = sut.readContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertEquals(3, result.size());
        for (int i = 0; i < result.size(); i++) {
            assertEquals(owner, result.get(i).getSubject());
            assertEquals(property, result.get(i).getAssertion());
            assertEquals(new Value<>(i + 1), result.get(i).getValue());
        }
    }

    @Test
    void persistContainerCreatesContainerAndAddsSpecifiedValuesToIt() throws Exception {
        this.sharedDataset = DatasetFactory.createTxnMem();
        connectorFactory.setDataset(sharedDataset);
        final Assertion property = Assertion.createObjectPropertyAssertion(URI.create("https://example.com/hasCandidates"), false);
        final List<NamedResource> values = List.of(NamedResource.create(Generator.generateUri()), NamedResource.create(Generator.generateUri()));
        final ContainerValueDescriptor<NamedResource> descriptor = ContainerValueDescriptor.bagValueDescriptor(owner, property);
        values.forEach(descriptor::addValue);

        connector.begin();
        sut.persistContainer(descriptor);
        connector.commit();
        verifyContainerContent(property, null, values);
    }

    private void verifyContainerContent(Assertion property, URI context, List<?> values) {
        final Resource subject = ResourceFactory.createResource(owner.getIdentifier().toString());
        final Property containerProperty = ResourceFactory.createProperty(property.getIdentifier().toString());
        final Model model = context != null ? sharedDataset.getNamedModel(context.toString()) : sharedDataset.getDefaultModel();
        final List<Statement> containerStatement = model.listStatements(subject, containerProperty, (RDFNode) null)
                                                        .toList();
        assertEquals(1, containerStatement.size());
        final Resource container = containerStatement.get(0).getObject().asResource();
        assertEquals(values.size(), model.listStatements(container, null, (RDFNode) null).toList().stream()
                                         .filter(Predicate.not(s -> s.getPredicate().equals(RDF.type))).toList()
                                         .size());
        for (int i = 0; i < values.size(); i++) {
            final RDFNode v = values.get(i) instanceof NamedResource u ? ResourceFactory.createResource(u.toString()) : ResourceFactory.createTypedLiteral(values.get(i));
            assertTrue(model.contains(container, ResourceFactory.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#_" + (i + 1)), v));
        }
    }

    @Test
    void persistContainerDoesNothingWhenValuesAreEmpty() throws Exception {
        this.sharedDataset = DatasetFactory.createTxnMem();
        connectorFactory.setDataset(sharedDataset);
        final Assertion property = Assertion.createObjectPropertyAssertion(URI.create("https://example.com/hasCandidates"), false);
        final ContainerValueDescriptor<NamedResource> descriptor = ContainerValueDescriptor.bagValueDescriptor(owner, property);

        connector.begin();
        sut.persistContainer(descriptor);
        connector.commit();
        assertTrue(sharedDataset.getDefaultModel().isEmpty());
    }

    @Test
    void persistContainerCreatesContainerAndAddsSpecifiedValuesToItInContext() throws Exception {
        this.sharedDataset = DatasetFactory.createTxnMem();
        connectorFactory.setDataset(sharedDataset);
        final Assertion property = Assertion.createObjectPropertyAssertion(URI.create("https://example.com/hasCandidates"), false);
        final List<NamedResource> values = List.of(NamedResource.create(Generator.generateUri()), NamedResource.create(Generator.generateUri()));
        final URI context = Generator.generateUri();
        final ContainerValueDescriptor<NamedResource> descriptor = ContainerValueDescriptor.bagValueDescriptor(owner, property, context);
        values.forEach(descriptor::addValue);

        connector.begin();
        sut.persistContainer(descriptor);
        connector.commit();
        verifyContainerContent(property, context, values);
    }

    @Test
    void updateContainerAppendsNewElementsToExistingContainer() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        loadData(ttl);
        final ContainerValueDescriptor<Integer> descriptor = ContainerValueDescriptor.seqValueDescriptor(owner, property);
        IntStream.range(1, 4).forEach(descriptor::addValue);

        connector.begin();
        sut.updateContainer(descriptor);
        connector.commit();
        final Resource containerIri = ResourceFactory.createResource("https://example.com/hasIsolationLevels/container");
        final Model model = sharedDataset.getDefaultModel();
        assertTrue(model.contains(ResourceFactory.createResource(owner.toString()), ResourceFactory.createProperty(property.getIdentifier()
                                                                                                                           .toString()), containerIri));
        verifyContainerContent(property, null, descriptor.getValues());
    }

    @Test
    void updateContainerRemovesAndAddsElementsToExistingContainer() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "3"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        loadData(ttl);
        final ContainerValueDescriptor<Integer> descriptor = ContainerValueDescriptor.seqValueDescriptor(owner, property);
        IntStream.range(1, 10).filter(i -> i % 2 == 0).forEach(descriptor::addValue);

        connector.begin();
        sut.updateContainer(descriptor);
        connector.commit();
        final Resource containerIri = ResourceFactory.createResource("https://example.com/hasIsolationLevels/container");
        final Model model = sharedDataset.getDefaultModel();
        assertTrue(model.contains(ResourceFactory.createResource(owner.toString()), ResourceFactory.createProperty(property.getIdentifier()
                                                                                                                           .toString()), containerIri));
        verifyContainerContent(property, null, descriptor.getValues());
    }

    @Test
    void updateContainerRemovesContainerAndItsContentWhenValuesAreEmpty() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "3"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        loadData(ttl);
        final ContainerValueDescriptor<Integer> descriptor = ContainerValueDescriptor.seqValueDescriptor(owner, property);

        connector.begin();
        sut.updateContainer(descriptor);
        connector.commit();
        final Resource containerIri = ResourceFactory.createResource("https://example.com/hasIsolationLevels/container");
        assertFalse(sharedDataset.getDefaultModel()
                                 .contains(ResourceFactory.createResource(owner.toString()), ResourceFactory.createProperty(property.getIdentifier()
                                                                                                                                    .toString()), containerIri));
        assertFalse(sharedDataset.getDefaultModel().contains(containerIri, null, (RDFNode) null));
    }
}
