package cz.cvut.kbss.ontodriver.rdf4j.container;

import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
import cz.cvut.kbss.ontodriver.exception.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnection;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ContainerHandlerTest {

    private Repository repository;

    private ContainerHandler sut;

    @BeforeEach
    void setUp() throws Exception {
        this.repository = new SailRepository(new MemoryStore());
        repository.init();
        final StorageConnector connectorMock = mock(StorageConnector.class);
        when(connectorMock.acquireConnection()).thenAnswer(inv -> repository.getConnection());
        this.sut = new ContainerHandler(new StorageConnection(connectorMock, null), repository.getValueFactory());
    }

    @Test
    void loadContainerReturnsEmptyListWhenContainerDoesNotExist() throws Exception {
        final NamedResource owner = NamedResource.create(Generator.generateUri());
        final Assertion property = Assertion.createDataPropertyAssertion(URI.create("https://example.com/hasIsolationLevels"), false);
        final List<Axiom<?>> result = sut.loadContainer(ContainerDescriptor.seqDescriptor(owner, property));
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void loadContainerReturnsListOfAxiomsRepresentingContainerContent() throws Exception {
        final NamedResource owner = NamedResource.create(Generator.generateUri());
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
        final NamedResource owner = NamedResource.create(Generator.generateUri());
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
        final NamedResource owner = NamedResource.create(Generator.generateUri());
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
    void loadContainerThrowsIntegrityConstraintViolatedExceptionWhenContainerValueIsNotUnique() throws Exception {
        final NamedResource owner = NamedResource.create(Generator.generateUri());
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
}
