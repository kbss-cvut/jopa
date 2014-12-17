package cz.cvut.kbss.ontodriver.unit;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntoDriver;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.impl.SimpleDataSource;
import cz.cvut.kbss.ontodriver.utils.DriverFactoryStub;

public class SimpleDataSourceTest {

	private static OntologyStorageProperties props;
	private static Map<String, String> properties;

	@Mock
	private OntoDriver driverMock;

	@Mock
	private PersistenceProviderFacade facadeMock;

	@Mock
	private StorageModule moduleMock;

	private SimpleDataSource ds;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		// Override the default factories
		props = new OntologyStorageProperties(URI.create("http://testOntology"),
				URI.create("file:testResults/ontoDriverTests.owl"), OntologyConnectorType.OWLAPI);
		properties = new HashMap<>();
		properties.put(OntoDriverProperties.OWLAPI_DRIVER_FACTORY,
				DriverFactoryStub.class.getName());
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		this.ds = new SimpleDataSource(props, properties);
		final Field f = SimpleDataSource.class.getDeclaredField("driver");
		f.setAccessible(true);
		f.set(ds, driverMock);
		when(driverMock.acquireStorageModule(facadeMock)).thenReturn(moduleMock);
	}

	@Test
	public void testSimpleDataSourceSingleArg() {
		final DataSource res = new SimpleDataSource(props);
		assertNotNull(res);
		assertTrue(res.isOpen());
	}

	@Test(expected = NullPointerException.class)
	public void testSimpleDataSourceSingleArgEmpty() {
		final DataSource res = new SimpleDataSource(null);
	}

	@Test
	public void testSimpleDataSourceTwoArgs() {
		final DataSource res = new SimpleDataSource(props, properties);
		assertNotNull(res);
		assertTrue(res.isOpen());
	}

	@Test
	public void testSimpleDataSourceTwoArgsNullProps() {
		final DataSource res = new SimpleDataSource(props, null);
		assertNotNull(res);
		assertTrue(res.isOpen());
	}

	@Test(expected = NullPointerException.class)
	public void testSimpleDataSourceTwoArgsNullStorages() throws Exception {
		final DataSource res = new SimpleDataSource(null, properties);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testGetConnection() throws Exception {
		final Connection res = ds.getConnection();
		// This shouldn't be reached
		assert res == null;
	}

	@Test
	public void testGetConnectionWithFacade() throws Exception {
		final Connection res = ds.getConnection(facadeMock);
		assertNotNull(res);
		assertTrue(res.isOpen());
		assertTrue(res.getAutoCommit());
		verify(driverMock).acquireStorageModule(facadeMock);
	}

	@Test
	public void testGetConnectionAutoCommitFalse() throws Exception {
		final Map<String, String> p = new HashMap<>(properties);
		p.put(OntoDriverProperties.CONNECTION_AUTO_COMMIT, Boolean.FALSE.toString());
		final DataSource src = new SimpleDataSource(props, p);
		final Field f = SimpleDataSource.class.getDeclaredField("driver");
		f.setAccessible(true);
		f.set(src, driverMock);

		final Connection res = src.getConnection(facadeMock);
		assertNotNull(res);
		assertFalse(res.getAutoCommit());
	}

	@Test(expected = NullPointerException.class)
	public void testGetConnectionNull() throws Exception {
		try {
			final Connection res = ds.getConnection(null);
			// This shouldn't be reached
			assert res == null;
		} finally {
			verify(driverMock, never()).acquireStorageModule(any(PersistenceProviderFacade.class));
		}
	}

	@Test
	public void testClose() throws Exception {
		assertTrue(ds.isOpen());
		ds.close();
		assertFalse(ds.isOpen());
		verify(driverMock).close();
	}
}
