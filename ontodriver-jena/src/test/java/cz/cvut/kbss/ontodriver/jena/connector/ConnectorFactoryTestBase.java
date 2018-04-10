package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public abstract class ConnectorFactoryTestBase {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    abstract ConnectorFactory connectorFactory(Configuration configuration);

    abstract SharedStorageConnector getCentralConnector(ConnectorFactory factory) throws Exception;

    @Test
    public void closeClosesCentralConnector() throws Exception {
        final Configuration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = connectorFactory(configuration);
        assertTrue(getCentralConnector(factory).isOpen());
        factory.close();
        assertFalse(factory.isOpen());
        assertFalse(getCentralConnector(factory).isOpen());
    }

    @Test
    public void createConnectorOnClosedFactoryThrowsIllegalStateException() throws Exception {
        thrown.expect(IllegalStateException.class);
        final Configuration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = connectorFactory(configuration);
        factory.close();
        assertFalse(factory.isOpen());
        factory.createConnector();
    }

    @Test
    public void createInferredConnectorReturnsCorrectConnector() throws Exception {
        final Configuration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = connectorFactory(configuration);
        final StorageConnector connector = factory.createConnector();
        final InferredStorageConnector result = factory.createInferredConnector(connector);
        assertTrue(result instanceof DummyInferredStorageConnector);
    }
}
