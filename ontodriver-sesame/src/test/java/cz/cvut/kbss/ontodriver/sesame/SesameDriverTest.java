package cz.cvut.kbss.ontodriver.sesame;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver_new.Connection;

public class SesameDriverTest {

	private static OntologyStorageProperties storageProperties;
	private static Map<String, String> properties;

	@Mock
	private ConnectorFactory connectorFactoryMock;

	@Mock
	private Connector connectorMock;

	private SesameDriver driver;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		storageProperties = OntologyStorageProperties.connectorType(OntologyConnectorType.SESAME)
				.ontologyUri(URI.create("http://ontology.org"))
				.physicalUri(URI.create("http://krizik.felk.cvut.cz/repo")).build();
		properties = Collections.emptyMap();
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(connectorFactoryMock.isOpen()).thenReturn(Boolean.TRUE);
		when(connectorFactoryMock.createStorageConnector(storageProperties, properties))
				.thenReturn(connectorMock);
		final Field instanceField = ConnectorFactory.class.getDeclaredField("instance");
		instanceField.setAccessible(true);
		instanceField.set(null, connectorFactoryMock);

		this.driver = new SesameDriver(storageProperties, properties);
	}

	@Test
	public void testClose() throws Exception {
		assertTrue(driver.isOpen());
		driver.close();
		assertFalse(driver.isOpen());
		verify(connectorFactoryMock).close();
	}

	@Test
	public void acquiresConnection() throws Exception {
		final Connection res = driver.acquireConnection();
		assertNotNull(res);
		assertNotNull(res.lists());
		verify(connectorFactoryMock).createStorageConnector(storageProperties, properties);
		verify(connectorFactoryMock).createStorageConnector(storageProperties, properties);
	}

	@Test
	public void removesClosedConnectionFromActiveConnections() throws Exception {
		final Connection conn = driver.acquireConnection();
		assertNotNull(conn);
		final Field connectionsField = SesameDriver.class.getDeclaredField("openedConnections");
		connectionsField.setAccessible(true);
		final Set<Connection> openedConnections = (Set<Connection>) connectionsField.get(driver);
		assertTrue(openedConnections.contains(conn));
		conn.close();
		assertFalse(openedConnections.contains(conn));
	}
}
