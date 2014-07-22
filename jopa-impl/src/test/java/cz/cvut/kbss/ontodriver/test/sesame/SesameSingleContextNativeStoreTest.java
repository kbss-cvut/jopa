package cz.cvut.kbss.ontodriver.test.sesame;

import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.test.utils.SesameNativeStorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.test.BaseSingleContextOntoDriverTest;
import cz.cvut.kbss.ontodriver.test.TestEnv;

public class SesameSingleContextNativeStoreTest extends BaseSingleContextOntoDriverTest {

	private static final Map<String, String> properties = initProperties();

	private static SesameSingleContextTests tests;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		tests = new SesameSingleContextTests(LOG);
		facade = TestEnv.getProviderFacade();
		storageConfig = new SesameNativeStorageConfig();
	}

	@After
	public void tearDown() throws Exception {
		if (c != null) {
			c.close();
		}
		ds.close();
		tests.entityE.setUri(null);
		tests.entityA.setStringAttribute(SesameSingleContextTests.A_STRING_ATT);
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
	public void testUpdateDataPropertyToNull() throws Exception {
		acquireConnection("UpdateDataPropertyToNull");
		tests.updateDataPropertyToNull();
	}

	@Test
	public void testRemove() throws Exception {
		acquireConnection("RemoveEntity");
		tests.testRemove();
	}

	@Override
	protected void acquireConnection(String ontoName) throws OntoDriverException {
		this.ds = TestEnv.createDataSource(ontoName, storageConfig, properties);
		this.c = ds.getConnection(facade);
		tests.setConnection(c);
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> m = new HashMap<>();
		m.put(OWLAPIPersistenceProperties.LANG, "en");
		m.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
		m.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		return m;
	}

}
