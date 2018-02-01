package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.util.Vocabulary;
import org.apache.jena.graph.Graph;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.junit.Assert.*;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

public class SharedStorageConnectorTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void initializationCreatesStorageAccessor() {
        final SharedStorageConnector connector = initConnector();
        assertNotNull(connector.storage);
        assertTrue(connector.isOpen());
    }

    private SharedStorageConnector initConnector() {
        final Configuration configuration = StorageTestUtil.createConfiguration("test:uri");
        final SharedStorageConnector connector = new SharedStorageConnector(configuration);
        connector.storage = spy(connector.storage);
        return connector;
    }

    @Test
    public void findFiltersStatementsFromDatasetDefaultModel() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
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
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);

        final Collection<Statement> result = connector
                .find(ResourceFactory.createResource(RESOURCE), null, null, NAMED_GRAPH);
        assertFalse(result.isEmpty());
    }

    @Test
    public void findInContextReturnsEmptyCollectionForUnknownContext() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);
        final Collection<Statement> result = connector
                .find(ResourceFactory.createResource(RESOURCE), null, null, "http://unknownGraph");
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    public void containsChecksForStatementExistenceInDefaultGraph() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.unwrap(Dataset.class);
        generateTestData(ds);
        assertTrue(connector.contains(null, ResourceFactory.createProperty(Vocabulary.RDF_TYPE),
                ResourceFactory.createResource(TYPE_ONE)));
    }

    @Test
    public void containsChecksForStatementExistenceInTargetNamedGraph() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
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

    @Test
    public void addAddsStatementsToDataset() {
        final SharedStorageConnector connector = initConnector();
        final Statement statement = ResourceFactory.createStatement(ResourceFactory.createResource(RESOURCE),
                ResourceFactory.createProperty(Vocabulary.RDF_TYPE), ResourceFactory.createResource(TYPE_TWO));
        assertFalse(connector.storage.getDataset().getDefaultModel().contains(statement));
        connector.begin();
        connector.add(Collections.singletonList(statement));
        assertTrue(connector.storage.getDataset().getDefaultModel().contains(statement));
    }

    @Test
    public void addAddsStatementsToTargetModelInDataset() {
        final SharedStorageConnector connector = initConnector();
        final Statement statement = ResourceFactory.createStatement(ResourceFactory.createResource(RESOURCE),
                ResourceFactory.createProperty(Vocabulary.RDF_TYPE), ResourceFactory.createResource(TYPE_TWO));
        assertFalse(connector.storage.getDataset().getNamedModel(NAMED_GRAPH).contains(statement));
        connector.begin();
        connector.add(Collections.singletonList(statement), NAMED_GRAPH);
        assertTrue(connector.storage.getDataset().getNamedModel(NAMED_GRAPH).contains(statement));
    }

    @Test
    public void removeRemovesStatementsFromDataset() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);
        final Statement statement = ResourceFactory.createStatement(ResourceFactory.createResource(RESOURCE),
                ResourceFactory.createProperty(Vocabulary.RDF_TYPE), ResourceFactory.createResource(TYPE_ONE));
        assertTrue(connector.storage.getDataset().getDefaultModel().contains(statement));
        connector.begin();
        connector.remove(Collections.singletonList(statement));
        assertFalse(connector.storage.getDataset().getDefaultModel().contains(statement));
    }

    @Test
    public void removeRemovesStatementsFromTargetModelInDataset() {
        final SharedStorageConnector connector = initConnector();
        final Dataset ds = connector.storage.getDataset();
        generateTestData(ds);
        final Statement statement = ResourceFactory.createStatement(ResourceFactory.createResource(RESOURCE),
                ResourceFactory.createProperty(Vocabulary.RDF_TYPE), ResourceFactory.createResource(TYPE_TWO));
        assertTrue(connector.storage.getDataset().getNamedModel(NAMED_GRAPH).contains(statement));
        connector.begin();
        connector.remove(Collections.singletonList(statement), NAMED_GRAPH);
        assertFalse(connector.storage.getDataset().getNamedModel(NAMED_GRAPH).contains(statement));
    }

    @Test
    public void beginStartsDatasetWriteTransaction() {
        final SharedStorageConnector connector = initConnector();
        connector.begin();
        assertTrue(connector.storage.getDataset().isInTransaction());
    }

    @Test
    public void commitCommitsDatasetWriteTransaction() throws Exception {
        final SharedStorageConnector connector = initConnector();
        connector.begin();
        assertTrue(connector.storage.getDataset().isInTransaction());
        final Statement statement = ResourceFactory.createStatement(ResourceFactory.createResource(RESOURCE),
                ResourceFactory.createProperty(Vocabulary.RDF_TYPE), ResourceFactory.createResource(TYPE_TWO));
        connector.add(Collections.singletonList(statement));
        connector.commit();
        assertFalse(connector.storage.getDataset().isInTransaction());
        assertTrue(connector.storage.getDataset().getDefaultModel().contains(statement));
    }

    @Test
    public void commitWritesChangesToUnderlyingRepository() throws Exception {
        final SharedStorageConnector connector = initConnector();
        connector.begin();
        final Statement statement = ResourceFactory.createStatement(ResourceFactory.createResource(RESOURCE),
                ResourceFactory.createProperty(Vocabulary.RDF_TYPE), ResourceFactory.createResource(TYPE_TWO));
        connector.add(Collections.singletonList(statement));
        connector.commit();
        verify(connector.storage).writeChanges();
    }

    @Test
    public void rollbackRollsBackChangesInDataset() {
        final SharedStorageConnector connector = initConnector();
        connector.begin();
        final Statement statement = ResourceFactory.createStatement(ResourceFactory.createResource(RESOURCE),
                ResourceFactory.createProperty(Vocabulary.RDF_TYPE), ResourceFactory.createResource(TYPE_TWO));
        connector.add(Collections.singletonList(statement));
        assertTrue(connector.storage.getDataset().getDefaultModel().contains(statement));
        connector.rollback();
        assertFalse(connector.storage.getDataset().getDefaultModel().contains(statement));
    }

    @Test
    public void getContextsListsContextInRepository() throws Exception {
        final SharedStorageConnector connector = initConnector();
        connector.begin();
        final String ctxOne = Generator.generateUri().toString();
        final Statement statementOne = ResourceFactory.createStatement(ResourceFactory.createResource(RESOURCE),
                ResourceFactory.createProperty(Vocabulary.RDF_TYPE), ResourceFactory.createResource(TYPE_ONE));
        final Statement statementTwo = ResourceFactory.createStatement(ResourceFactory.createResource(RESOURCE),
                ResourceFactory.createProperty(Vocabulary.RDF_TYPE), ResourceFactory.createResource(TYPE_TWO));
        connector.add(Collections.singletonList(statementOne), ctxOne);
        connector.add(Collections.singletonList(statementTwo), NAMED_GRAPH);
        connector.commit();
        final List<String> contexts = connector.getContexts();
        assertTrue(contexts.contains(ctxOne));
        assertTrue(contexts.contains(NAMED_GRAPH));
    }

    @Test
    public void getContextReturnsEmptyListWhenNoNamedGraphsArePresent() {
        final SharedStorageConnector connector = initConnector();
        final List<String> contexts = connector.getContexts();
        assertNotNull(contexts);
        assertTrue(contexts.isEmpty());
    }

    @Test
    public void unwrapReturnsConnectorInstanceWhenClassMatches() {
        final SharedStorageConnector connector = initConnector();
        final StorageConnector result = connector.unwrap(StorageConnector.class);
        assertSame(connector, result);
    }

    @Test
    public void unwrapReturnsDatasetInstanceWhenClassMatches() {
        final SharedStorageConnector connector = initConnector();
        final Dataset result = connector.unwrap(Dataset.class);
        assertSame(connector.storage.dataset, result);
    }

    @Test
    public void unwrapThrowsUnsupportedOperationExceptionWhenTargetClassIsNotSupported() {
        thrown.expect(UnsupportedOperationException.class);
        final SharedStorageConnector connector = initConnector();
        connector.unwrap(Graph.class);
    }
}