package cz.cvut.kbss.ontodriver.test.sesame;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.test.utils.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.test.TestEnv;

public class SesameSingleContextMemoryStoreTest {

	private static final Logger LOG = Logger.getLogger(SesameSingleContextMemoryStoreTest.class
			.getName());

	private static final List<StorageConfig> storageInfo = Collections
			.<StorageConfig> singletonList(new SesameMemoryStorageConfig());
	private static final Map<String, String> properties = initProperties();

	private static PersistenceProviderFacade facade;
	private DataSource ds;
	private Connection c;

	private static SesameSingleContextTests tests;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		tests = new SesameSingleContextTests(LOG);
		facade = TestEnv.getProviderFacade();
	}

	@After
	public void tearDown() throws Exception {
		if (c != null) {
			c.close();
		}
		ds.close();
		tests.entityE.setUri(null);
	}

	@Test
	public void testAcquireConnection() throws Exception {
		acquireConnection("AcquireConnectionTest");
		tests.testAcquireConnection();
	}

	@Test
	public void testPersistSimple() throws Exception {
		acquireConnection("PersistSimpleTest");
		tests.testPersistSimple();
	}

	@Test
	public void testPersistWithTypes() throws Exception {
		acquireConnection("PersistWithTypes");
		tests.testPersistWithTypes();
	}

	@Test
	public void testPersistWithIdGenerated() throws Exception {
		acquireConnection("PersistGeneratedId");
		tests.testPersistWithIdGenerated();
	}

	@Test
	public void testPersistWithIdGeneratedMultiple() throws Exception {
		acquireConnection("PersistGeneratedIdMultiple");
		tests.testPersistWithIdGeneratedMultiple();
	}

	@Test
	public void testPersistWithObjectProperty() throws Exception {
		acquireConnection("PersistObjectProperty");
		tests.testPersistWithObjectProperty();
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistTwice() throws Exception {
		acquireConnection("PersistTwice");
		tests.testPersistTwice();
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistTwiceInTransaction() throws Exception {
		acquireConnection("PersistTwiceInTransaction");
		tests.testPersistTwiceInTransaction();
	}

	@Test
	public void testUpdateDataPropertyValue() throws Exception {
		acquireConnection("UpdateDataProperty");
		tests.testUpdateDataPropertyValue();
	}

	@Test
	public void testUpdateTypes() throws Exception {
		acquireConnection("UpdateTypes");
		tests.testUpdateTypes();
	}

	@Test
	public void testUpdateObjectProperty() throws Exception {
		acquireConnection("UpdateObjectProperty");
		tests.testUpdateObjectProperty();
	}

	@Test
	public void testUpdateObjectPropertyToNull() throws Exception {
		acquireConnection("UpdateObjectPropertyToNull");
		tests.testUpdateObjectPropertyToNull();
	}

	@Test
	public void testRemove() throws Exception {
		acquireConnection("RemoveEntity");
		tests.testRemove();
	}

	private void acquireConnection(String ontoName) throws OntoDriverException {
		this.ds = TestEnv.createDataSource(ontoName, storageInfo, properties);
		this.c = ds.getConnection(facade);
		tests.setConnection(c);
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> m = new HashMap<>();
		m.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
		return m;
	}
}
