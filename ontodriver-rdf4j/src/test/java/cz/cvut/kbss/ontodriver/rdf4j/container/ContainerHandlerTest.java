package cz.cvut.kbss.ontodriver.rdf4j.container;

import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ContainerValueDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnection;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.sail.SailRepository;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.List;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ContainerHandlerTest {

    private final NamedResource owner = NamedResource.create(Generator.generateUri());
    private ValueFactory vf;

    private Repository repository;

    private StorageConnection storageConnection;

    private ContainerHandler sut;

    @BeforeEach
    void setUp() throws Exception {
        this.repository = new SailRepository(new MemoryStore());
        repository.init();
        this.vf = repository.getValueFactory();
        final StorageConnector connectorMock = mock(StorageConnector.class);
        when(connectorMock.acquireConnection()).thenAnswer(inv -> repository.getConnection());
        this.storageConnection = new StorageConnection(connectorMock, null);
        this.sut = new ContainerHandler(storageConnection, repository.getValueFactory());
    }

    @Test
    void loadContainerReturnsEmptyListWhenContainerDoesNotExist() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final List<Axiom<?>> result = sut.loadContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void loadContainerReturnsListOfAxiomsRepresentingContainerContent() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "3"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.add(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE);
        }

        final List<Axiom<?>> result = sut.loadContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertEquals(3, result.size());
        for (int i = 0; i < result.size(); i++) {
            assertEquals(owner, result.get(i).getSubject());
            assertEquals(property, result.get(i).getAssertion());
            assertEquals(new Value<>(i + 1), result.get(i).getValue());
        }
    }

    @Test
    void loadContainerReturnsListOfAxiomsWithDuplicateValuesRepresentingContainerContentWithDuplicates() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "1"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.add(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE);
        }

        final List<Axiom<?>> result = sut.loadContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertEquals(List.of(1, 2, 1), result.stream().map(a -> a.getValue().getValue()).toList());
    }

    @Test
    void loadContainerReturnsListOfAxiomsRepresentingContainerContentFromContext() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final URI context = Generator.generateUri();
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.add(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE, repository.getValueFactory()
                                                                                                 .createIRI(context.toString()));
        }

        final List<Axiom<?>> result = sut.loadContainer(ContainerDescriptor.seqDescriptor(owner, property, context));
        assertNotNull(result);
        assertEquals(2, result.size());
        for (int i = 0; i < result.size(); i++) {
            assertEquals(owner, result.get(i).getSubject());
            assertEquals(property, result.get(i).getAssertion());
            assertEquals(new Value<>(i + 1), result.get(i).getValue());
        }
    }

    @Test
    void loadContainerSupportsContainerRepresentedByBlankNode() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> _:xxx .
                _:xxx rdf:_1 "1"^^xsd:int .
                _:xxx rdf:_2 "2"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.add(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE);
        }

        final List<Axiom<?>> result = sut.loadContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void loadContainerThrowsIntegrityConstraintViolatedExceptionWhenContainerValueIsNotUnique() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "2"^^xsd:int .
                <%s> <%s> <https://example.com/hasIsolationLevels/anotherObject> .
                """.formatted(owner.toString(), property.getIdentifier(), owner.toString(), property.getIdentifier());
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.add(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE);
        }

        assertThrows(IntegrityConstraintViolatedException.class, () -> sut.loadContainer(ContainerDescriptor.seqDescriptor(owner, property)));
    }

    @Test
    void loadContainerPreservesOrderBasedOnContainerMembershipPropertiesNumbering() throws Exception {
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final String ttl = """
                @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
                @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
                <%s> <%s> <https://example.com/hasIsolationLevels/container> .
                <https://example.com/hasIsolationLevels/container> rdf:_1 "1"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_3 "3"^^xsd:int .
                <https://example.com/hasIsolationLevels/container> rdf:_2 "2"^^xsd:int .
                """.formatted(owner.toString(), property.getIdentifier());
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.add(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE);
        }

        final List<Axiom<?>> result = sut.loadContainer(ContainerDescriptor.seqDescriptor(owner, property));
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
        final Assertion property = Assertion.createObjectPropertyAssertion(URI.create("https://example.com/hasCandidates"), false);
        final List<NamedResource> values = List.of(NamedResource.create(Generator.generateUri()), NamedResource.create(Generator.generateUri()));
        final ContainerValueDescriptor<NamedResource> descriptor = ContainerValueDescriptor.bagValueDescriptor(owner, property);
        values.forEach(descriptor::addValue);

        storageConnection.begin();
        sut.persistContainer(descriptor);
        storageConnection.commit();
        verifyContainerContent(property, null, values);
    }

    private void verifyContainerContent(Assertion property, URI context, List<?> values) {
        final IRI subject = vf.createIRI(owner.getIdentifier().toString());
        final IRI containerProperty = vf.createIRI(property.getIdentifier().toString());
        final IRI contextIri = context != null ? vf.createIRI(context.toString()) : null;
        try (RepositoryConnection conn = repository.getConnection()) {
            final List<Statement> containerStatement = conn.getStatements(subject, containerProperty, null).stream()
                                                           .toList();
            assertEquals(1, containerStatement.size());
            final Resource container = (Resource) containerStatement.get(0).getObject();
            for (int i = 0; i < values.size(); i++) {
                final org.eclipse.rdf4j.model.Value v = values.get(i) instanceof NamedResource u ? vf.createIRI(u.toString()) : vf.createLiteral((int) values.get(i));
                if (contextIri != null) {
                    assertTrue(conn.hasStatement(container, vf.createIRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#_" + (i + 1)), v, false, contextIri));
                } else {
                    assertTrue(conn.hasStatement(container, vf.createIRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#_" + (i + 1)), v, false));
                }
            }
        }
    }

    @Test
    void persistContainerDoesNothingWhenValuesAreEmpty() throws Exception {
        final Assertion property = Assertion.createObjectPropertyAssertion(URI.create("https://example.com/hasCandidates"), false);
        final ContainerValueDescriptor<NamedResource> descriptor = ContainerValueDescriptor.bagValueDescriptor(owner, property);

        storageConnection.begin();
        sut.persistContainer(descriptor);
        storageConnection.commit();
        try (final RepositoryConnection conn = repository.getConnection()) {
            assertTrue(conn.isEmpty());
        }
    }

    @Test
    void persistContainerCreatesContainerAndAddsSpecifiedValuesToItInContext() throws Exception {
        final Assertion property = Assertion.createObjectPropertyAssertion(URI.create("https://example.com/hasCandidates"), false);
        final List<NamedResource> values = List.of(NamedResource.create(Generator.generateUri()), NamedResource.create(Generator.generateUri()));
        final URI context = Generator.generateUri();
        final ContainerValueDescriptor<NamedResource> descriptor = ContainerValueDescriptor.bagValueDescriptor(owner, property, context);
        values.forEach(descriptor::addValue);

        storageConnection.begin();
        sut.persistContainer(descriptor);
        storageConnection.commit();
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
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.add(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE);
        }
        final ContainerValueDescriptor<Integer> descriptor = ContainerValueDescriptor.seqValueDescriptor(owner, property);
        IntStream.range(1, 4).forEach(descriptor::addValue);
        storageConnection.begin();
        sut.updateContainer(descriptor);
        storageConnection.commit();
        final IRI containerIri = vf.createIRI("https://example.com/hasIsolationLevels/container");
        try (final RepositoryConnection conn = repository.getConnection()) {
            assertTrue(conn.hasStatement(vf.createIRI(owner.toString()), vf.createIRI(property.getIdentifier()
                                                                                              .toString()), containerIri, false));
        }
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
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.add(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE);
        }
        final ContainerValueDescriptor<Integer> descriptor = ContainerValueDescriptor.seqValueDescriptor(owner, property);
        IntStream.range(1, 10).filter(i -> i % 2 == 0).forEach(descriptor::addValue);
        storageConnection.begin();
        sut.updateContainer(descriptor);
        storageConnection.commit();
        final IRI containerIri = vf.createIRI("https://example.com/hasIsolationLevels/container");
        try (final RepositoryConnection conn = repository.getConnection()) {
            assertTrue(conn.hasStatement(vf.createIRI(owner.toString()), vf.createIRI(property.getIdentifier()
                                                                                              .toString()), containerIri, false));
        }
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
        try (final RepositoryConnection conn = repository.getConnection()) {
            conn.add(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE);
        }
        final ContainerValueDescriptor<Integer> descriptor = ContainerValueDescriptor.seqValueDescriptor(owner, property);
        storageConnection.begin();
        sut.updateContainer(descriptor);
        storageConnection.commit();
        final IRI containerIri = vf.createIRI("https://example.com/hasIsolationLevels/container");
        try (final RepositoryConnection conn = repository.getConnection()) {
            assertFalse(conn.hasStatement(vf.createIRI(owner.toString()), vf.createIRI(property.getIdentifier()
                                                                                              .toString()), containerIri, false));
            assertFalse(conn.hasStatement(containerIri, null, null, false));
        }
    }
}
