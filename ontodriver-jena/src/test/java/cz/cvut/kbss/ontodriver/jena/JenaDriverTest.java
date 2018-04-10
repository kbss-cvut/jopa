package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.jena.connector.InferenceConnectorFactory;
import cz.cvut.kbss.ontodriver.jena.connector.ReadCommittedConnectorFactory;
import cz.cvut.kbss.ontodriver.jena.connector.SnapshotConnectorFactory;
import org.apache.jena.reasoner.rulesys.RDFSRuleReasonerFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.*;

public class JenaDriverTest {

    private OntologyStorageProperties storageProps;
    private final Map<String, String> properties = new HashMap<>();

    private JenaDriver driver;

    @Before
    public void setUp() {
        this.storageProps = OntologyStorageProperties.driver(JenaDataSource.class.getName())
                                                     .physicalUri(URI.create("temp:memory")).build();
        properties.put(JenaOntoDriverProperties.JENA_STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        properties.put(JenaOntoDriverProperties.JENA_ISOLATION_STRATEGY, JenaOntoDriverProperties.READ_COMMITTED);
    }

    @After
    public void tearDown() throws Exception {
        if (driver != null && driver.isOpen()) {
            driver.close();
        }
    }

    @Test
    public void initCreatesReadCommittedConnectorFactoryWhenConfigured() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        assertNotNull(driver);
        assertTrue(driver.isOpen());
        assertTrue(getConnectorFactory() instanceof ReadCommittedConnectorFactory);
    }

    private ConnectorFactory getConnectorFactory() throws Exception {
        final Field factoryField = JenaDriver.class.getDeclaredField("connectorFactory");
        factoryField.setAccessible(true);
        return (ConnectorFactory) factoryField.get(driver);
    }

    @Test
    public void initCreatesSnapshotConnectorFactoryWhenConfigured() throws Exception {
        properties.put(JenaOntoDriverProperties.JENA_ISOLATION_STRATEGY, JenaOntoDriverProperties.SNAPSHOT);
        this.driver = new JenaDriver(storageProps, properties);
        assertNotNull(driver);
        assertTrue(driver.isOpen());
        assertTrue(getConnectorFactory() instanceof SnapshotConnectorFactory);
    }

    @Test
    public void initCreatesInferenceConnectorFactoryWhenReasonerFactoryIsConfigured() throws Exception {
        properties.put(OntoDriverProperties.REASONER_FACTORY_CLASS, RDFSRuleReasonerFactory.class.getName());
        this.driver = new JenaDriver(storageProps, properties);
        assertNotNull(driver);
        assertTrue(driver.isOpen());
        assertTrue(getConnectorFactory() instanceof InferenceConnectorFactory);
    }

    @Test
    public void acquireConnectionCreatesAndReturnsConnectionInstance() {
        this.driver = new JenaDriver(storageProps, properties);
        final JenaConnection connection = driver.acquireConnection();
        assertNotNull(connection);
        assertTrue(connection.isOpen());
    }

    @Test
    public void closeClosesAllOpenConnectionsAndConnectorFactory() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        final JenaConnection connection = driver.acquireConnection();
        assertTrue(connection.isOpen());
        assertTrue(driver.isOpen());
        driver.close();
        assertFalse(driver.isOpen());
        assertFalse(connection.isOpen());
        assertFalse(getConnectorFactory().isOpen());
    }

    @Test
    public void closingDriverTwiceDoesNothing() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        driver.acquireConnection();
        driver.close();
        driver.close();
        assertFalse(driver.isOpen());
    }

    @Test
    public void connectionClosedNotificationRemovesConnectionFromCollectionOfOpenConnections() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        final JenaConnection connection = driver.acquireConnection();
        final Field openConnectionsField = JenaDriver.class.getDeclaredField("openConnections");
        openConnectionsField.setAccessible(true);
        final Set openConnections = (Set) openConnectionsField.get(driver);
        assertTrue(openConnections.contains(connection));
        driver.connectionClosed(connection);
        assertFalse(openConnections.contains(connection));
    }

    @Test
    public void driverRegistersItselfAsListenerOnCreatedConnection() throws Exception {
        this.driver = new JenaDriver(storageProps, properties);
        final JenaConnection connection = driver.acquireConnection();
        final Field listenerField = JenaConnection.class.getDeclaredField("listener");
        listenerField.setAccessible(true);
        assertEquals(driver, listenerField.get(connection));
    }

    @Test
    public void driverCreatesAutoCommitConnectionsWhenConfiguredTo() {
        properties.put(OntoDriverProperties.CONNECTION_AUTO_COMMIT, Boolean.TRUE.toString());
        this.driver = new JenaDriver(storageProps, properties);
        final JenaConnection connection = driver.acquireConnection();
        assertTrue(connection.isAutoCommit());
    }
}