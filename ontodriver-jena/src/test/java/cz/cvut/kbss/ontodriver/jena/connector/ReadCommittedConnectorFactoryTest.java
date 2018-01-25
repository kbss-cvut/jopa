package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.lang.reflect.Field;

import static org.junit.Assert.*;

public class ReadCommittedConnectorFactoryTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void createConnectorCreatesNewChangeTrackingConnectorWrappingCentralConnector() throws Exception {
        final Configuration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = new ReadCommittedConnectorFactory(configuration);
        final StorageConnector connector = factory.createConnector();
        assertTrue(connector instanceof ChangeTrackingStorageConnector);
        assertNotNull(getCentralConnector(factory));
        assertTrue(getCentralConnector(factory).isOpen());
    }

    private SharedStorageConnector getCentralConnector(ConnectorFactory factory) throws Exception {
        final Field connectorField = ReadCommittedConnectorFactory.class.getDeclaredField("centralConnector");
        connectorField.setAccessible(true);
        return (SharedStorageConnector) connectorField.get(factory);
    }

    @Test
    public void closeClosesCentralConnector() throws Exception {
        final Configuration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = new ReadCommittedConnectorFactory(configuration);
        assertTrue(getCentralConnector(factory).isOpen());
        factory.close();
        assertFalse(factory.isOpen());
        assertFalse(getCentralConnector(factory).isOpen());
    }

    @Test
    public void createConnectorOnClosedFactoryThrowsIllegalStateException() throws Exception {
        thrown.expect(IllegalStateException.class);
        final Configuration configuration = StorageTestUtil.createConfiguration("test:uri");
        final ConnectorFactory factory = new ReadCommittedConnectorFactory(configuration);
        factory.close();
        assertFalse(factory.isOpen());
        factory.createConnector();
    }
}