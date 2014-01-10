package cz.cvut.kbss.ontodriver.unit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.StorageManager;
import cz.cvut.kbss.ontodriver.impl.OntoDriverImpl;
import cz.cvut.kbss.ontodriver.impl.jena.DriverJenaFactory;
import cz.cvut.kbss.ontodriver.impl.owlapi.DriverOwlapiFactory;
import cz.cvut.kbss.ontodriver.utils.DriverFactoryMock;
import cz.cvut.kbss.ontodriver.utils.MetamodelMock;
import cz.cvut.kbss.ontodriver.utils.PersistenceProviderMock;

public class OntoDriverImplTest {

	private static final Logger LOG = Logger.getLogger(OntoDriverImplTest.class.getName());

	private static List<OntologyStorageProperties> props;
	private static Metamodel metamodelMock;

	private static OntoDriverImpl driver;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		DriverFactoryMock.init(new DriverFactoryMock());
		// Override the default factories
		props = new ArrayList<OntologyStorageProperties>();
		props.add(new OntologyStorageProperties(URI.create("http://testOntology"), URI
				.create("file:testResults/ontoDriverTests.owl"), OntologyConnectorType.OWLAPI));
		metamodelMock = new MetamodelMock();
	}

	@Before
	public void setUp() throws Exception {
		driver = new OntoDriverImpl(props);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testOntoDriverImplListOfOntologyStorageProperties() {
		LOG.config("Test: constructor, null passed.");
		final OntoDriverImpl d = new OntoDriverImpl(null);
		fail("This line should not have been reached.");
		d.isOpen();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testOntoDriverImplListOfOntologyStoragePropertiesMapOfProperties() {
		LOG.config("Test: constructor, empty list of storage properties passed.");
		final OntoDriverImpl d = new OntoDriverImpl(
				Collections.<OntologyStorageProperties> emptyList(),
				Collections.<String, String> emptyMap());
		fail("This line should not have been reached.");
		d.isOpen();
	}

	@Test
	public void testOntoDriverImplConstructor() throws Exception {
		LOG.config("Test: constructor with properties.");
		final Map<String, String> map = Collections.singletonMap(
				OntoDriverProperties.CONNECTION_AUTO_COMMIT, "false");
		final OntoDriverImpl d = new OntoDriverImpl(props, map);
		assertNotNull(d);
		assertNotNull(d.acquireStorageManager());
	}

	@Test
	public void testAcquireStorageManager() throws Exception {
		LOG.config("Test: acquire manager. No parameters.");
		final StorageManager res = driver.acquireStorageManager();
		assertNotNull(res);
		assertEquals(props.size(), res.getAvailableContexts().size());
		assertEquals(props.get(0).getOntologyURI(), res.getAvailableContexts().get(0).getUri());
	}

	@Test
	public void testAcquireStorageManagerMetamodel() throws Exception {
		LOG.config("Test: acquire manager. With metamodel.");
		final StorageManager res = driver.acquireStorageManager(metamodelMock);
		assertNotNull(res);
		assertEquals(props.size(), res.getAvailableContexts().size());
		assertEquals(props.get(0).getOntologyURI(), res.getAvailableContexts().get(0).getUri());
	}

	@Test(expected = NullPointerException.class)
	public void testAcquireStorageManagerMetamodelNull() throws Exception {
		LOG.config("Test: acquire manager. With metamodel. Null passed as metamodel.");
		@SuppressWarnings("unused")
		final StorageManager res = driver.acquireStorageManager((Metamodel) null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testAcquireStorageManagerPersistenceProviderFacade() throws Exception {
		LOG.config("Test: acquire manager. With PersistenceProviderFacade.");
		final PersistenceProviderFacade f = new PersistenceProviderMock();
		final StorageManager res = driver.acquireStorageManager(f);
		assertNotNull(res);
		assertEquals(props.size(), res.getAvailableContexts().size());
		assertEquals(props.get(0).getOntologyURI(), res.getAvailableContexts().get(0).getUri());
	}

	@Test(expected = NullPointerException.class)
	public void testAcquireStorageManagerPersistenceProviderFacadeNull() throws Exception {
		LOG.config("Test: acquire manager. With PersistenceProviderFacade. Null passed as PersistenceProviderFacade.");
		@SuppressWarnings("unused")
		final StorageManager res = driver.acquireStorageManager((PersistenceProviderFacade) null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testGetFactory() throws Exception {
		LOG.config("Test: get factory for context.");
		final StorageManager m = driver.acquireStorageManager();
		for (Context ctx : m.getAvailableContexts()) {
			final DriverFactory f = driver.getFactory(ctx);
			assertNotNull(f);
			switch (ctx.getConnectorType()) {
			case OWLAPI:
				assertTrue(f instanceof DriverOwlapiFactory);
				break;
			case JENA:
				assertTrue(f instanceof DriverJenaFactory);
				break;
			}
		}
	}

	@Test(expected = NullPointerException.class)
	public void testGetFactoryNull() {
		LOG.config("Test: get factory for context. Null passed as context.");
		@SuppressWarnings("unused")
		final DriverFactory f = driver.getFactory(null);
		fail("This line should not have been reached.");
	}
}
