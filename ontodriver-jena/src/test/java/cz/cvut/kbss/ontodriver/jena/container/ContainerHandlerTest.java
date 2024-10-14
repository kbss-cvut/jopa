package cz.cvut.kbss.ontodriver.jena.container;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.descriptor.ContainerDescriptor;
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
import org.apache.jena.riot.RDFFormat;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ContainerHandlerTest {

    private final NamedResource owner = NamedResource.create(Generator.generateUri());

    private ConnectorFactory connectorFactory;

    private StorageConnector connector;

    private ContainerHandler sut;

    @BeforeEach
    void setUp() {
        final DriverConfiguration config = new DriverConfiguration(OntologyStorageProperties.driver(JenaDataSource.class.getName()).physicalUri("mem:test").build());
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
        final Dataset dataset = DatasetFactory.createTxnMem();
        dataset.begin(ReadWrite.WRITE);
        dataset.getDefaultModel().read(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE.getLang().getLabel());
        dataset.commit();
        connectorFactory.setDataset(dataset);
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
        final Dataset dataset = DatasetFactory.createTxnMem();
        dataset.begin(ReadWrite.WRITE);
        dataset.getNamedModel(context.toString()).read(new ByteArrayInputStream(ttl.getBytes()), null, RDFFormat.TURTLE.getLang().getLabel());
        dataset.commit();
        connectorFactory.setDataset(dataset);

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
    void readContainerThrowsIntegrityConstraintViolatedExceptionWhenContainerValueIsNotUnique() throws Exception {
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
}
