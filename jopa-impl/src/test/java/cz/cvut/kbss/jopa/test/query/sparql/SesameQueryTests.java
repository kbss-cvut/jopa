package cz.cvut.kbss.jopa.test.query.sparql;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.query.env.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.utils.SesameNativeStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

public class SesameQueryTests {

	private static final Logger LOG = Logger.getLogger(MultiContextQueryTest.class.getName());

	private static final Map<String, String> properties = initProperties();
	private static final List<StorageConfig> storages = initStorages();
	private static final QueryExecutor ex = new QueryExecutor();

	private static EntityManagerFactory emf;

	private EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		TestEnvironment.clearDatabase();
		TestEnvironment.resetOwldbHibernateProvider();
		final EntityManager em = TestEnvironment.getPersistenceConnector("SesameQueryTests",
				storages, true, properties);
		QueryTestEnvironment.generateTestData(em);
		emf = em.getEntityManagerFactory();
		em.close();
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		emf.close();
		TestEnvironment.clearDatabase();
	}

	@Before
	public void setUp() throws Exception {
		emf.getCache().evictAll();
		this.em = emf.createEntityManager();
	}

	@After
	public void tearDown() throws Exception {
		em.close();
	}

	// --------------------
	//
	// This part uses Query
	//
	// --------------------

	@Test
	public void testSelectFromSingleContext() {
		LOG.config("Test: select subjects by type from a single context.");
		ex.testSelectFromSingleContext(em);
	}

	@Test
	public void testSelectByDataProperty() {
		LOG.config("Test: select data property values from all contexts.");
		ex.testSelectByDataProperty(em);
	}

	@Test
	public void testSelectByObjectProperty() {
		LOG.config("Test: select subjects having the same object property value.");
		ex.testSelectByObjectProperty(em);
	}

	@Test
	public void testSelectTypes() {
		LOG.config("Test: select types of a subject.");
		ex.testSelectTypes(em);
	}

	// -------------------------
	//
	// This part uses TypedQuery
	//
	// -------------------------

	@Test
	public void testSelectInstancesOfClass_Typed() {
		LOG.config("Test: select all instances of a single class. Typed query.");
		ex.testSelectInstancesOfClass_Typed(em);
	}

	@Test
	public void testSelectByTypeAndDataPropertyValue_Typed() {
		LOG.config("Test: select an instance by class and data property value. Typed query.");
		ex.testSelectByTypeAndDataPropertyValue_Typed(em);
	}

	@Test
	public void testSelectByObjectProperty_Typed() {
		LOG.config("Test: select instances by object property. Using setMaxResults. Typed query.");
		ex.testSelectByObjectProperty_Typed(em);
	}

	private static List<StorageConfig> initStorages() {
		final List<StorageConfig> lst = new ArrayList<>(2);
		lst.add(new SesameNativeStorageConfig());
		return lst;
	}

	private static Map<String, String> initProperties() {
		final Map<String, String> props = new HashMap<>();
		props.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.TRUE.toString());
		props.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
		props.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
		return props;
	}

}
