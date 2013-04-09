package cz.cvut.kbss.jopa.jpa.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

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
import cz.cvut.kbss.jopa.model.OWLEntityExistsException;
import cz.cvut.kbss.jopa.ontodriver.TestEnv;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.OWLClassE;
import cz.cvut.kbss.jopa.owlapi.OWLClassG;
import cz.cvut.kbss.jopa.owlapi.OWLClassH;
import cz.cvut.kbss.jopa.owlapi.OWLClassI;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.jopa.owlapi.utils.StorageInfo;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public class JpaPersistOperationsTest {

	private static final Logger LOG = Logger.getLogger(JpaPersistOperationsTest.class.getName());

	private static final List<StorageInfo> storages = initStorages();
	private static OWLClassA entityA;
	private static OWLClassB entityB;
	private static OWLClassD entityD;
	// Generated IRI
	private static OWLClassE entityE;
	// Lazy reference to OWLClassA
	private static OWLClassI entityI;
	// Two relationships
	private static OWLClassG entityG;
	private static OWLClassH entityH;

	private static EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entityA = new OWLClassA();
		entityA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityA"));
		entityA.setStringAttribute("entityAStringAttribute");
		final Set<String> types = new HashSet<String>();
		types.add("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassU");
		entityA.setTypes(types);
		entityB = new OWLClassB();
		entityB.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityB"));
		entityB.setStringAttribute("entityBStringAttribute");
		entityD = new OWLClassD();
		entityD.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityD"));
		entityD.setOwlClassA(entityA);
		entityE = new OWLClassE();
		entityE.setStringAttribute("entityEStringAttribute");
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
		entityE.setUri(null);
	}

	@Test
	public void testPersistIntoAllCached() {
		LOG.config("Test: test persist entities into all contexts. Cached.");
		em = TestEnvironment.getPersistenceConnector("JpaIntegration-PersistIntoAllCached",
				storages, true);
		final List<Context> contexts = em.getAvailableContexts();
		assertNotNull(contexts);
		assertEquals(storages.size(), contexts.size());
		assertTrue(contexts.size() > 2);
		final Context ctxA = contexts.get(0);
		final Context ctxB = contexts.get(1);
		final Context ctxE = contexts.get(2);
		assertNull(entityE.getUri());
		em.getTransaction().begin();
		em.persist(entityA, ctxA.getUri());
		em.persist(entityB, ctxB.getUri());
		em.persist(entityE, ctxE.getUri());
		assertTrue(em.contains(entityA));
		assertTrue(em.contains(entityB));
		assertTrue(em.contains(entityE));
		assertNotNull(entityE.getUri());
		em.getTransaction().commit();

		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getUri(), resA.getUri());
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
		assertEquals(entityA.getTypes().size(), resA.getTypes().size());
		assertTrue(entityA.getTypes().containsAll(resA.getTypes()));
		final OWLClassB resB = em.find(OWLClassB.class, entityB.getUri(), ctxB.getUri());
		assertNotNull(resB);
		assertEquals(entityB.getStringAttribute(), resB.getStringAttribute());
		final OWLClassE resE = em.find(OWLClassE.class, entityE.getUri());
		assertNotNull(resE);
		assertEquals(entityE.getUri(), resE.getUri());
		assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
	}

	@Test
	public void testPersistIntoAllNotCached() {
		LOG.config("Test: test persist entities into all contexts. Not cached.");
		em = TestEnvironment.getPersistenceConnector("JpaIntegration-PersistIntoAllNotCached",
				storages, false);
		final List<Context> contexts = em.getAvailableContexts();
		assertNotNull(contexts);
		assertEquals(storages.size(), contexts.size());
		assertTrue(contexts.size() > 2);
		final Context ctxA = contexts.get(0);
		final Context ctxB = contexts.get(1);
		final Context ctxE = contexts.get(2);
		assertNull(entityE.getUri());
		em.getTransaction().begin();
		em.persist(entityA, ctxA.getUri());
		em.persist(entityB, ctxB.getUri());
		em.persist(entityE, ctxE.getUri());
		assertTrue(em.contains(entityA));
		assertTrue(em.contains(entityB));
		assertTrue(em.contains(entityE));
		assertNotNull(entityE.getUri());
		em.getTransaction().commit();

		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		assertEquals(entityA.getUri(), resA.getUri());
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
		assertEquals(entityA.getTypes().size(), resA.getTypes().size());
		assertTrue(entityA.getTypes().containsAll(resA.getTypes()));
		final OWLClassB resB = em.find(OWLClassB.class, entityB.getUri(), ctxB.getUri());
		assertNotNull(resB);
		assertEquals(entityB.getStringAttribute(), resB.getStringAttribute());
		final OWLClassE resE = em.find(OWLClassE.class, entityE.getUri());
		assertNotNull(resE);
		assertEquals(entityE.getUri(), resE.getUri());
		assertEquals(entityE.getStringAttribute(), resE.getStringAttribute());
	}

	@Test
	public void testPersistTheSameIntoAll() {
		LOG.config("Test: persist entity with the same primary key into all available contexts.");
		em = TestEnvironment.getPersistenceConnector("JpaIntegration-PersistSamePrimaryKeyIntoAll",
				storages, false);
		final List<Context> contexts = em.getAvailableContexts();
		assertFalse(contexts.isEmpty());
		em.getTransaction().begin();
		final String attBase = "StringAttributeB";
		int i = 1;
		for (Context ctx : contexts) {
			final OWLClassB b = new OWLClassB();
			b.setUri(entityB.getUri());
			b.setStringAttribute(attBase + (i++));
			em.persist(b, ctx.getUri());
		}
		em.getTransaction().commit();
		em.clear();
		i = 1;
		for (Context ctx : contexts) {
			final OWLClassB resB = em.find(OWLClassB.class, entityB.getUri(), ctx.getUri());
			assertNotNull(resB);
			final String expected = attBase + (i++);
			assertEquals(expected, resB.getStringAttribute());
		}
	}

	@Test
	public void testPersistCascade() {
		LOG.config("Test: persist cascaded relationship.");
		em = TestEnvironment.getPersistenceConnector("JpaIntegration-PersistCascade", storages,
				false);
		final List<Context> contexts = em.getAvailableContexts();
		assertFalse(contexts.isEmpty());
		assertTrue(contexts.size() > 1);
		final Context ctx = contexts.get(1);
		em.getTransaction().begin();
		em.persist(entityI, ctx.getUri());
		em.persist(entityB);
		assertTrue(em.contains(entityB));
		assertTrue(em.contains(entityI));
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();

		em.clear();
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		final OWLClassI resI = em.find(OWLClassI.class, entityI.getUri(), ctx.getUri());
		assertNotNull(resI);
		final OWLClassB resB = em.find(OWLClassB.class, entityB.getUri());
		assertNotNull(resB);
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistDuplicate() {
		LOG.config("Test: persist entity twice into the same context.");
		em = TestEnvironment.getPersistenceConnector("JpaIntegration-PersistDuplicate", storages,
				false);
		final List<Context> contexts = em.getAvailableContexts();
		assertFalse(contexts.isEmpty());
		final Context ctx = contexts.get(contexts.size() - 1);
		em.getTransaction().begin();
		em.persist(entityA, ctx.getUri());
		em.persist(entityA, ctx.getUri());
		fail("This line should not have been reached.");
		// The transaction is rolled back by tearDown
	}

	@Test
	public void testPersistCascadeOverTwo() {
		LOG.config("Test: persist with cascade over two relationships.");
		em = TestEnvironment.getPersistenceConnector("JpaIntegration-PersistCascadeOverTwo",
				storages, false);
		final List<Context> contexts = em.getAvailableContexts();
		assertFalse(contexts.isEmpty());
		final Context ctx = contexts.get(0);
		em.getTransaction().begin();
		em.persist(entityG, ctx.getUri());
		assertTrue(em.contains(entityG));
		assertTrue(em.contains(entityH));
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();
		em.clear();

		final OWLClassG resG = em.find(OWLClassG.class, entityG.getUri(), ctx.getUri());
		assertNotNull(resG);
		assertNotNull(resG.getOwlClassH());
		final OWLClassH resH = em.find(OWLClassH.class, entityH.getUri(), ctx.getUri());
		assertNotNull(resH);
		assertEquals(resG.getOwlClassH(), resH);
		assertNotNull(resH.getOwlClassA());
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri(), ctx.getUri());
		assertNotNull(resA);
		assertEquals(resH.getOwlClassA(), resA);
		assertEquals(entityA.getStringAttribute(), resA.getStringAttribute());
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
