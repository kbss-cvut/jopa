package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import org.apache.jena.vocabulary.RDF;
import org.junit.Before;
import org.junit.Test;

import static cz.cvut.kbss.ontodriver.jena.connector.StorageTestUtil.*;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.spy;

public class DummyInferredStorageConnectorTest {

    private StorageConnector wrappedConnector;

    private InferredStorageConnector connector;

    @Before
    public void setUp() {
        final Configuration configuration = StorageTestUtil.createConfiguration("test:uri");
        final SharedStorageConnector wrapped = new SharedStorageConnector(configuration);
        wrapped.storage = spy(wrapped.storage);
        this.wrappedConnector = wrapped;
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
}