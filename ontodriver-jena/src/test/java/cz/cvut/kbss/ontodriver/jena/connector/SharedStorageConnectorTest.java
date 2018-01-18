package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.junit.Test;

import java.util.Collection;

import static org.junit.Assert.*;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

public class SharedStorageConnectorTest {

    private static final String RESOURCE = "http://onto.fel.cvut.cz/ontologies/jena-driver/Resource";
    private static final String TYPE_ONE = "http://onto.fel.cvut.cz/ontologies/jena-driver/TypeOne";
    private static final String TYPE_TWO = "http://onto.fel.cvut.cz/ontologies/jena-driver/TypeTwo";
    private static final String NAMED_GRAPH = "http://onto.fel.cvut.cz/ontologies/jena-driver/GraphOne";

    @Test
    public void initializationCreatesStorageAccessor() {
        final SharedStorageConnector connector = initConnector();
        assertNotNull(connector.storage);
        assertTrue(connector.isOpen());
    }

    private SharedStorageConnector initConnector() {
        final Configuration configuration = StorageTestBase.createConfiguration("test:uri");
        final SharedStorageConnector connector = new SharedStorageConnector(configuration);
        connector.storage = spy(connector.storage);
        return connector;
    }

    @Test
    public void findFiltersStatementsFromDatasetDefaultModel() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.unwrap(Storage.class).getDataset();
        generateTestData(ds);

        final Collection<Statement> result = connector.find(ResourceFactory.createResource(RESOURCE), null, null);
        assertFalse(result.isEmpty());
    }

    private void generateTestData(Dataset dataset) {
        final Model m = dataset.getDefaultModel();
        m.add(m.createResource(RESOURCE), m.getProperty(Vocabulary.RDF_TYPE), m.createResource(TYPE_ONE));
        final Model namedGraph = ModelFactory.createDefaultModel();
        namedGraph.add(m.createResource(RESOURCE), m.getProperty(Vocabulary.RDF_TYPE), m.createResource(TYPE_TWO));
        dataset.addNamedModel(NAMED_GRAPH, namedGraph);
    }

    @Test
    public void findInContextFiltersStatementsInTargetContext() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.unwrap(Storage.class).getDataset();
        generateTestData(ds);

        final Collection<Statement> result = connector
                .find(ResourceFactory.createResource(RESOURCE), null, null, NAMED_GRAPH);
        assertFalse(result.isEmpty());
    }

    @Test
    public void findInContextReturnsEmptyCollectionForUnknownContext() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.unwrap(Storage.class).getDataset();
        generateTestData(ds);
        final Collection<Statement> result = connector
                .find(ResourceFactory.createResource(RESOURCE), null, null, "http://unknownGraph");
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    public void containsChecksForStatementExistenceInDefaultGraph() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.unwrap(Storage.class).getDataset();
        generateTestData(ds);
        assertTrue(connector.contains(null, ResourceFactory.createProperty(Vocabulary.RDF_TYPE),
                ResourceFactory.createResource(TYPE_ONE)));
    }

    @Test
    public void containsChecksForStatementExistenceInTargetNamedGraph() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.unwrap(Storage.class).getDataset();
        generateTestData(ds);
        assertTrue(connector.contains(null, null, ResourceFactory.createResource(TYPE_TWO), NAMED_GRAPH));
    }

    @Test
    public void closeClosesUnderlyingStorageAccessor() {
        final SharedStorageConnector connector = initConnector();
        assertTrue(connector.isOpen());
        connector.close();
        assertFalse(connector.isOpen());
        verify(connector.storage).close();
    }
}