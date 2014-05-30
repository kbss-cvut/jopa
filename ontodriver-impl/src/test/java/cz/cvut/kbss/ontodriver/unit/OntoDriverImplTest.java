package cz.cvut.kbss.ontodriver.unit;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntoDriver;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.impl.OntoDriverImpl;
import cz.cvut.kbss.ontodriver.utils.DriverFactoryStub;

public class OntoDriverImplTest {

	private static OntologyStorageProperties storageProperties;
	private static Map<String, String> properties;

	@Mock
	private Metamodel metamodelMock;

	@Mock
	private StorageModule moduleMock;

	private OntoDriverImpl driver;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		// Override the default factories
		storageProperties = new OntologyStorageProperties(URI.create("http://testOntology"),
				URI.create("file:testResults/ontoDriverTests.owl"), OntologyConnectorType.OWLAPI);
		properties = new HashMap<>();
		properties.put(OntoDriverProperties.OWLAPI_DRIVER_FACTORY,
				DriverFactoryStub.class.getName());
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		driver = new OntoDriverImpl(storageProperties, properties);
		DriverFactoryStub.instance.setStorageModule(moduleMock);
	}

	@Test(expected = NullPointerException.class)
	public void testOntoDriverImplNull() {
		final OntoDriverImpl d = new OntoDriverImpl(null);
		assert d == null;
	}

	@Test
	public void testOntoDriverImplNullProperties() {
		final OntoDriverImpl d = new OntoDriverImpl(storageProperties, null);
		assertNotNull(d);
	}

	@Test
	public void testOntoDriverImplNoProperties() throws Exception {
		final OntoDriver d = new OntoDriverImpl(storageProperties);
		assertNotNull(d);
		assertTrue(d.isOpen());
	}

	@Test
	public void testOntoDriverImplConstructor() throws Exception {
		properties.put(OntoDriverProperties.CONNECTION_AUTO_COMMIT, "false");
		final OntoDriverImpl d = new OntoDriverImpl(storageProperties, properties);
		assertNotNull(d);
	}

	@Test(expected = OntoDriverInitializationException.class)
	public void testOntoDriverImplFactoryNameEmpty() throws Exception {
		final Map<String, String> m = new HashMap<>(properties);
		m.put(OntoDriverProperties.JENA_DRIVER_FACTORY, "");
		final OntologyStorageProperties props = OntologyStorageProperties
				.connectorType(OntologyConnectorType.JENA)
				.physicalUri(URI.create("http://localhost:8080/store")).build();
		final OntoDriver d = new OntoDriverImpl(props, m);
		// This shouldn't be reached
		assert d == null;
	}

	@Test(expected = OntoDriverInitializationException.class)
	public void testOntoDriverImplUnknownFactoryClass() throws Exception {
		final Map<String, String> m = new HashMap<>(properties);
		m.put(OntoDriverProperties.JENA_DRIVER_FACTORY, "SomeUnknownClass");
		final OntologyStorageProperties props = OntologyStorageProperties
				.connectorType(OntologyConnectorType.JENA)
				.physicalUri(URI.create("http://localhost:8080/store")).build();
		final OntoDriver d = new OntoDriverImpl(props, m);
		// This shouldn't be reached
		assert d == null;
	}

	@Test(expected = OntoDriverInitializationException.class)
	public void testOntodriverImplFactoryNotAssignable() throws Exception {
		final Map<String, String> m = new HashMap<>(properties);
		m.put(OntoDriverProperties.JENA_DRIVER_FACTORY, Connection.class.getName());
		final OntologyStorageProperties props = OntologyStorageProperties
				.connectorType(OntologyConnectorType.JENA)
				.physicalUri(URI.create("http://localhost:8080/store")).build();
		final OntoDriver d = new OntoDriverImpl(props, m);
		// This shouldn't be reached
		assert d == null;
	}

	@Test
	public void testClose() throws Exception {
		assertTrue(driver.isOpen());
		driver.close();
		assertFalse(driver.isOpen());
		assertFalse(DriverFactoryStub.instance.isOpen());
	}

	@Test
	public void testAcquireStorageModule() throws Exception {
		final StorageModule res = driver.acquireStorageModule();
		assertNotNull(res);
		assertSame(moduleMock, res);
	}

	@Test
	public void testAcquireStorageModuleMetamodel() throws Exception {
		final StorageModule res = driver.acquireStorageModule(metamodelMock);
		assertNotNull(res);
		assertSame(moduleMock, res);
	}

	@Test(expected = NullPointerException.class)
	public void testAcquireStorageModuleMetamodelNull() throws Exception {
		final StorageModule res = driver.acquireStorageModule((Metamodel) null);
		// This shouldn't be reached
		assert res == null;
	}

	@Test
	public void testAcquireStorageModulePPFacade() throws Exception {
		final PersistenceProviderFacade ppf = mock(PersistenceProviderFacade.class);
		final StorageModule res = driver.acquireStorageModule(ppf);
		assertNotNull(res);
		assertSame(moduleMock, res);
	}

	@Test(expected = NullPointerException.class)
	public void testAcquireStorageModulePPFacadeNull() throws Exception {
		final StorageModule res = driver.acquireStorageModule((PersistenceProviderFacade) null);
		// This shouldn't be reached
		assert res == null;
	}
}
