package cz.cvut.kbss.jopa.jpa.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.net.URI;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.ontodriver.TestEnv;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.OWLClassG;
import cz.cvut.kbss.jopa.owlapi.OWLClassH;
import cz.cvut.kbss.jopa.owlapi.OWLClassI;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.jopa.owlapi.utils.StorageInfo;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public class JpaRetrieveOperationsTest {

	private static final Logger LOG = Logger.getLogger(JpaRetrieveOperationsTest.class.getName());

	private static final List<StorageInfo> storages = initStorages();
	private static final String OWLCLASSA_REFERNCE = "owlClassA";

	private static OWLClassA entityA;
	private static OWLClassD entityD;
	private static OWLClassG entityG;
	private static OWLClassH entityH;
	private static OWLClassI entityI;

	private static EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		final Set<String> types = new HashSet<String>();
		types.add("OWLClassA");
		entityA.setTypes(types);
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
		entityD.setOwlClassA(entityA);
		entityI = new OWLClassI();
		entityI.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityI"));
		entityI.setOwlClassA(entityA);
		entityH = new OWLClassH();
		entityH.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityH"));
		entityH.setOwlClassA(entityA);
		entityG = new OWLClassG();
		entityG.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityG"));
		entityG.setOwlClassH(entityH);
	}

	@Before
	public void setUp() throws Exception {
		clearDatabase();
	}

	@After
	public void tearDown() throws Exception {
		if (em.isOpen()) {
			if (em.getTransaction().isActive()) {
				em.getTransaction().rollback();
			}
			em.close();
			em.getEntityManagerFactory().close();
		}
	}

	@Test
	public void testRetrieveFirstAvailable() {
		LOG.config("Test: retrieve first available entity with the specified primary key.");
		em = TestEnvironment.getPersistenceConnector("RetrieveFirstAvailable", storages, false);
		final List<Context> contexts = em.getAvailableContexts();
		assertFalse(contexts.isEmpty());
		final Context ctx = contexts.get(contexts.size() - 1);
		em.getTransaction().begin();
		em.persist(entityA, ctx.getUri());
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();
		em.clear();

		final OWLClassA res = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(res);
		assertEquals(entityA.getUri(), res.getUri());
		assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
		assertEquals(entityA.getTypes().size(), res.getTypes().size());
		assertTrue(entityA.getTypes().containsAll(res.getTypes()));
	}

	@Test
	public void testRetrieveFromAll() {
		LOG.config("Test: persist entity into all contexts and retrieve from all.");
		em = TestEnvironment.getPersistenceConnector("RetrieveFromAll", storages, false);
		final List<Context> contexts = em.getAvailableContexts();
		em.getTransaction().begin();
		for (Context ctx : contexts) {
			em.persist(entityA, ctx.getUri());
			em.persist(entityD, ctx.getUri());
		}
		em.getTransaction().commit();
		em.clear();

		final List<OWLClassD> results = new ArrayList<OWLClassD>(contexts.size());
		for (Context ctx : contexts) {
			final OWLClassD resD = em.find(OWLClassD.class, entityD.getUri(), ctx.getUri());
			assertNotNull(resD);
			assertNotNull(resD.getOwlClassA());
			for (OWLClassD d : results) {
				assertFalse(d == resD);
			}
			results.add(resD);
		}
	}

	@Test
	public void testRetrieveLazyLoad() throws Exception {
		LOG.config("Test: retrieve entity with lazily loaded field.");
		em = TestEnvironment.getPersistenceConnector("RetrieveLazyLoad", storages, false);
		em.getTransaction().begin();
		// Persist with cascade
		em.persist(entityI);
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();
		em.clear();

		final OWLClassI resI = em.find(OWLClassI.class, entityI.getUri());
		assertNotNull(resI);
		final Field refA = OWLClassI.class.getDeclaredField(OWLCLASSA_REFERNCE);
		refA.setAccessible(true);
		// Before lazy loading
		assertNull(refA.get(resI));
		assertNotNull(resI.getOwlClassA());
		// After lazy loading
		assertNotNull(refA.get(resI));
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		assertEquals(resA, resI.getOwlClassA());
	}

	@Test
	public void testRetreiveRelationship() {
		LOG.config("Test: retrieve three entities connected by two relationships.");
		em = TestEnvironment.getPersistenceConnector("RetrieveRelationship", storages, false);
		final Context ctx = em.getAvailableContexts().get(0);
		em.getTransaction().begin();
		em.persist(entityG, ctx.getUri());
		assertTrue(em.contains(entityH));
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();
		em.clear();

		final OWLClassG resG = em.find(OWLClassG.class, entityG.getUri(), ctx.getUri());
		assertNotNull(resG);
		assertNotNull(resG.getOwlClassH());
		assertNotNull(resG.getOwlClassH().getOwlClassA());
	}

	@Test(expected = NullPointerException.class)
	public void testRetrieveNullPk() {
		LOG.config("Test: retrieve null passed as primary key.");
		em = TestEnvironment.getPersistenceConnector("RetrieveNullPk", storages, false);
		em.find(OWLClassA.class, null);
		fail("This line should not have been reached.");
	}

	@Test(expected = OWLPersistenceException.class)
	public void testRetrieveUnknownContext() {
		LOG.config("Test: retrieve, unknown context uri passed.");
		em = TestEnvironment.getPersistenceConnector("RetrieveUnknownContext", storages, false);
		final URI unknownContext = URI.create("http://unknownContext");
		em.find(OWLClassA.class, entityA.getUri(), unknownContext);
		fail("This line should not have been reached.");
	}

	@Test
	public void testRetrieveNotFound() {
		LOG.config("Test: retrieve entity which is not in the specified context.");
		em = TestEnvironment.getPersistenceConnector("RetrieveNotFound", storages, false);
		final List<Context> contexts = em.getAvailableContexts();
		assertFalse(contexts.isEmpty());
		final Context empty = contexts.get(0);
		final Context target = contexts.get(contexts.size() - 1);
		em.getTransaction().begin();
		em.persist(entityI, target.getUri());
		em.getTransaction().commit();
		em.clear();

		final OWLClassI resI = em.find(OWLClassI.class, entityI.getUri(), empty.getUri());
		assertNull(resI);
	}

	private static void clearDatabase() throws Exception {
		java.sql.Connection con = null;
		Statement st1 = null;
		Statement st2 = null;
		ResultSet rs = null;
		con = DriverManager.getConnection(TestEnv.DB_URI, TestEnv.DB_USERNAME, TestEnv.DB_PASSWORD);
		st1 = con.createStatement();
		rs = st1.executeQuery("SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'");
		final String deleteStmt = "TRUNCATE ";
		while (rs.next()) {
			final String table = rs.getString(1);
			st2 = con.createStatement();
			st2.executeUpdate(deleteStmt + table + " CASCADE");
			st2.close();
			st2 = null;
		}
		st1.close();
		con.close();
	}

	private static List<StorageInfo> initStorages() {
		final List<StorageInfo> lst = new ArrayList<StorageInfo>(3);
		lst.add(new StorageInfo(OntologyConnectorType.OWLAPI, OwlapiStorageType.FILE));
		lst.add(new StorageInfo(OntologyConnectorType.OWLAPI, OwlapiStorageType.OWLDB));
		lst.add(new StorageInfo(OntologyConnectorType.JENA, OwlapiStorageType.FILE));
		return lst;
	}
}
