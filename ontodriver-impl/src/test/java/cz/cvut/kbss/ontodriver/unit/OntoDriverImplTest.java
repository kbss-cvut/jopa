package cz.cvut.kbss.ontodriver.unit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.OntoDriver;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.impl.OntoDriverImpl;
import cz.cvut.kbss.ontodriver.utils.DriverFactoryStub;

public class OntoDriverImplTest {

	private static List<OntologyStorageProperties> props;
	private static Map<String, String> properties;

	@Mock
	private Metamodel metamodelMock;

	private OntoDriverImpl driver;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		// Override the default factories
		props = new ArrayList<OntologyStorageProperties>();
		props.add(new OntologyStorageProperties(URI.create("http://testOntology"), URI
				.create("file:testResults/ontoDriverTests.owl"), OntologyConnectorType.OWLAPI));
		properties = new HashMap<>();
		properties.put(OntoDriverProperties.OWLAPI_DRIVER_FACTORY,
				DriverFactoryStub.class.getName());
	}

	@Before
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		driver = new OntoDriverImpl(props, properties);
	}

	@Test(expected = NullPointerException.class)
	public void testOntoDriverImplNull() {
		final OntoDriverImpl d = new OntoDriverImpl(null);
		assert d == null;
	}

	@Test(expected = IllegalArgumentException.class)
	public void testOntoDriverImplNoRepositories() {
		final OntoDriverImpl d = new OntoDriverImpl(
				Collections.<OntologyStorageProperties> emptyList(),
				Collections.<String, String> emptyMap());
		assert d == null;
	}

	@Test
	public void testOntoDriverImplNoProperties() throws Exception {
		final OntoDriver d = new OntoDriverImpl(props);
		assertNotNull(d);
		assertTrue(d.isOpen());
	}

	@Test
	public void testOntoDriverImplConstructor() throws Exception {
		properties.put(OntoDriverProperties.CONNECTION_AUTO_COMMIT, "false");
		final OntoDriverImpl d = new OntoDriverImpl(props, properties);
		assertNotNull(d);
		assertNotNull(d.acquireStorageManager());
	}

	@Test(expected = OntoDriverInitializationException.class)
	public void testOntoDriverImplFactoryNameEmpty() throws Exception {
		final Map<String, String> m = new HashMap<>(properties);
		m.put(OntoDriverProperties.JENA_DRIVER_FACTORY, "");
		final OntoDriver d = new OntoDriverImpl(props, m);
		// This shouldn't be reached
		assert d == null;
	}

	@Test(expected = OntoDriverInitializationException.class)
	public void testOntodriverImplFactoryNotAssignable() throws Exception {
		final Map<String, String> m = new HashMap<>(properties);
		m.put(OntoDriverProperties.JENA_DRIVER_FACTORY, Connection.class.getName());
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
	public void testAcquireStorageManagerMetamodel() throws Exception {
		final StorageManager sm = driver.acquireStorageManager(metamodelMock);
		assertNotNull(sm);
		assertEquals(props.size(), sm.getRepositories().size());
	}

	@Test(expected = NullPointerException.class)
	public void testAcquireStorageManagerMetamodelNull() throws Exception {
		final StorageManager res = driver.acquireStorageManager((Metamodel) null);
		// This shouldn't be reached
		assert res == null;
	}

	@Test
	public void testAcquireStorageManagerPPFacade() throws Exception {
		final PersistenceProviderFacade ppf = mock(PersistenceProviderFacade.class);
		final StorageManager res = driver.acquireStorageManager(ppf);
		assertNotNull(res);
		assertFalse(res.getRepositories().isEmpty());
	}

	@Test(expected = NullPointerException.class)
	public void testAcquireStorageManagerPPFacadeNull() throws Exception {
		final StorageManager res = driver.acquireStorageManager((PersistenceProviderFacade) null);
		// This shouldn't be reached
		assert res == null;
	}

	@Test
	public void testGetFactory() throws Exception {
		final Field r = OntoDriverImpl.class.getDeclaredField("repositories");
		r.setAccessible(true);
		@SuppressWarnings("unchecked")
		final List<Repository> reps = (List<Repository>) r.get(driver);
		assertEquals(1, reps.size());
		final Repository re = reps.get(0);
		final DriverFactory f = driver.getFactory(re);
		assertNotNull(f);
		assertSame(DriverFactoryStub.instance, f);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testGetFactoryUnknown() throws Exception {
		final Repository r = new Repository(URI.create("http://unknown"));
		final DriverFactory f = driver.getFactory(r);
		// This shouldn't be reached
		assert f == null;
	}

	@Test(expected = NullPointerException.class)
	public void testGetFactoryNull() {
		final DriverFactory f = driver.getFactory(null);
		// This shouldn't be reached
		assert f == null;
	}
}
