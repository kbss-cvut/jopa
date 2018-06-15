package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import org.junit.Test;

import java.lang.reflect.Field;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class SnapshotConnectorFactoryTest extends ConnectorFactoryTestBase {

    @Override
    ConnectorFactory connectorFactory(DriverConfiguration configuration) {
        return new SnapshotConnectorFactory(configuration);
    }

    @Override
    SharedStorageConnector getCentralConnector(ConnectorFactory factory) throws Exception {
        final Field connectorField = SharedConnectorBasedConnectorFactory.class.getDeclaredField("centralConnector");
        connectorField.setAccessible(true);
        return (SharedStorageConnector) connectorField.get(factory);
    }

    @Test
    public void createConnectorCreatesNewSnapshotBasedStorageConnector() throws Exception {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = connectorFactory(configuration);
        final StorageConnector connector = factory.createConnector();
        assertTrue(connector instanceof SnapshotStorageConnector);
        assertNotNull(getCentralConnector(factory));
        assertTrue(getCentralConnector(factory).isOpen());
    }
}