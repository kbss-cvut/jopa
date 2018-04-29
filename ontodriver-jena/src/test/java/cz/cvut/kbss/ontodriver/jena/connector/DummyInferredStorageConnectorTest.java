package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.query.AbstractResultSet;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.vocabulary.RDF;
import org.junit.Before;
import org.junit.Test;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.Assert.*;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

public class DummyInferredStorageConnectorTest {

    private StorageConnector wrappedConnector;

    private InferredStorageConnector connector;

    @Before
    public void setUp() {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        final SharedStorageConnector wrapped = new SharedStorageConnector(configuration);
        wrapped.storage = spy(wrapped.storage);
        this.wrappedConnector = spy(wrapped);
        generateTestData(wrapped.storage.getDataset());
        this.connector = new DummyInferredStorageConnector(wrappedConnector);
    }

    @Test
    public void containsWithInferenceByDefaultUsesRegularContains() {
        assertEquals(wrappedConnector.contains(RESOURCE, RDF.type, createResource(TYPE_ONE), null),
                connector.containsWithInference(RESOURCE, RDF.type, createResource(TYPE_ONE), null));
    }

    @Test
    public void containsWithInferenceInContextByDefaultUsesRegularContainsInContext() {
        assertEquals(wrappedConnector.contains(RESOURCE, RDF.type, createResource(TYPE_TWO), NAMED_GRAPH),
                connector.containsWithInference(RESOURCE, RDF.type, createResource(TYPE_TWO), NAMED_GRAPH));
    }

    @Test
    public void findWithInferenceByDefaultUsesRegularFind() {
        assertEquals(wrappedConnector.find(RESOURCE, RDF.type, null, null),
                connector.findWithInference(RESOURCE, RDF.type, null, null));
    }

    @Test
    public void findWithInferenceInContextByDefaultUsesRegularFindInContext() {
        assertEquals(wrappedConnector.find(RESOURCE, RDF.type, null, NAMED_GRAPH),
                connector.findWithInference(RESOURCE, RDF.type, null, NAMED_GRAPH));
    }

    @Test
    public void isConsistentReturnsTrue() {
        assertTrue(connector.isConsistent(null));
    }

    @Test
    public void isContextConsistentReturnsTrue() {
        assertTrue(connector.isConsistent(NAMED_GRAPH));
    }

    @Test
    public void executeSelectQueryForwardsCallToWrappedConnector() throws Exception {
        final Query query = QueryFactory.create("SELECT * WHERE { ?x ?y ?z . }");
        final AbstractResultSet resultSet =
                connector.executeSelectQuery(query, Statement.StatementOntology.TRANSACTIONAL);
        assertNotNull(resultSet);
        verify(wrappedConnector).executeSelectQuery(query, Statement.StatementOntology.TRANSACTIONAL);
    }

    @Test
    public void executeAskQueryForwardsCallToWrappedConnector() throws Exception {
        final Query query = QueryFactory.create("ASK { ?x a <" + TYPE_ONE + "> . }");
        final AbstractResultSet resultSet = connector.executeAskQuery(query, Statement.StatementOntology.CENTRAL);
        assertNotNull(resultSet);
        verify(wrappedConnector).executeAskQuery(query, Statement.StatementOntology.CENTRAL);
    }

    @Test
    public void executeUpdateQueryForwardsCallToWrappedConnector() throws Exception {
        final String query = "INSERT DATA { _:b1 a <" + TYPE_ONE + "> . }";
        connector.executeUpdate(query, Statement.StatementOntology.TRANSACTIONAL);
        verify(wrappedConnector).executeUpdate(query, Statement.StatementOntology.TRANSACTIONAL);
    }
}