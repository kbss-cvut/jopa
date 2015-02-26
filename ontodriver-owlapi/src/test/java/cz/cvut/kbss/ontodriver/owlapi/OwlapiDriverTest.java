package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver_new.Connection;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;

import static org.junit.Assert.*;

public class OwlapiDriverTest {

    private static final OntologyStorageProperties STORAGE_PROPERTIES = OntologyStorageProperties.connectorType(
            OntologyConnectorType.OWLAPI).ontologyUri(
            URI.create("http://krizik.felk.cvut.cz/ontologies/jopa")).physicalUri(
            URI.create("http://example.com")).build();

    @Mock
    private Connector connectorMock;

    private OwlapiDriver driver;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final Field connectorField = ConnectorFactory.class.getDeclaredField("connector");
        connectorField.setAccessible(true);
        connectorField.set(ConnectorFactory.getInstance(STORAGE_PROPERTIES, Collections.<String, String>emptyMap()),
                connectorMock);

        this.driver = new OwlapiDriver(STORAGE_PROPERTIES, Collections.<String, String>emptyMap());
    }

    @AfterClass
    public static void tearDownAfterClass() throws Exception {
        final Field instanceField = ConnectorFactory.class.getDeclaredField("instance");
        instanceField.setAccessible(true);
        instanceField.set(null, null);
    }

    @Test
    public void createsNewConnectionsForAllRequests() throws Exception {
        final Connection cOne = driver.acquireConnection();
        assertNotNull(cOne);
        final Connection cTwo = driver.acquireConnection();
        assertNotNull(cTwo);
        assertNotSame(cOne, cTwo);
    }

    @Test
    public void closesConnectionsOnClose() throws Exception {
        final Connection cOne = driver.acquireConnection();
        assertTrue(cOne.isOpen());
        final Connection cTwo = driver.acquireConnection();
        assertTrue(cTwo.isOpen());

        driver.close();
        assertFalse(cOne.isOpen());
        assertFalse(cTwo.isOpen());
    }
}
