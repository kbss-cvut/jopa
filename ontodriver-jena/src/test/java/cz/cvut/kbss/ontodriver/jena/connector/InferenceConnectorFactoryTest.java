package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import org.junit.Test;

import java.lang.reflect.Field;
import java.util.Collections;

import static org.junit.Assert.assertTrue;

public class InferenceConnectorFactoryTest extends ConnectorFactoryTestBase {

    @Override
    ConnectorFactory connectorFactory(DriverConfiguration configuration) {
        return new InferenceConnectorFactory(configuration, Collections.emptyMap());
    }

    @Override
    SharedStorageConnector getCentralConnector(ConnectorFactory factory) throws Exception {
        final Field connectorField = InferenceConnectorFactory.class.getDeclaredField("centralConnector");
        connectorField.setAccessible(true);
        return (SharedStorageConnector) connectorField.get(factory);
    }

    @Test
    public void createInferredConnectorReturnsCorrectConnector() {
        final DriverConfiguration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = connectorFactory(configuration);
        final StorageConnector connector = factory.createConnector();
        final InferredStorageConnector result = factory.createInferredConnector(connector);
        assertTrue(result instanceof SnapshotStorageConnectorWithInference);
    }
}