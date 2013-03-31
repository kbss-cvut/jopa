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
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.ontodriver.TestEnv;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassC;
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

public class JpaUpdateOperationsTest {

	private static final Logger LOG = Logger.getLogger(JpaUpdateOperationsTest.class.getName());

	private static final List<StorageInfo> storages = initStorages();

	private static OWLClassA entityA;
	private static OWLClassC entityC;
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
		entityC = new OWLClassC();
		entityC.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/entityC"));
		final List<OWLClassA> simpleList = new ArrayList<OWLClassA>(1);
		simpleList.add(entityA);
		entityC.setSimpleList(simpleList);
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
	public void testUpdateTypes() {
		LOG.config("Test: add type to types collection.");
		em = TestEnvironment.getPersistenceConnector("UpdateTypes", storages, true);
		final Context ctx = em.getAvailableContexts().get(0);
		em.getTransaction().begin();
		em.persist(entityA, ctx.getUri());
		em.getTransaction().commit();

		final String added = OWLClassB.class.getName();
		final OWLClassA mod = em.find(OWLClassA.class, entityA.getUri());
		em.getTransaction().begin();
		mod.getTypes().add(added);
		em.getTransaction().commit();

		final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), ctx.getUri());
		assertNotNull(res);
		assertEquals(entityA.getTypes().size() + 1, res.getTypes().size());
		assertTrue(res.getTypes().contains(added));
		assertTrue(res.getTypes().containsAll(entityA.getTypes()));
	}

	@Test
	public void testMergeDetached() {
		LOG.config("Test: merge detached entity.");
		em = TestEnvironment.getPersistenceConnector("MergeDetached", storages, false);
		em.getTransaction().begin();
		em.persist(entityA);
		em.getTransaction().commit();
		em.clear();

		final OWLClassA detached = new OWLClassA();
		detached.setUri(entityA.getUri());
		detached.setTypes(entityA.getTypes());
		detached.setStringAttribute("SomeOtherStringAttribute");
		em.getTransaction().begin();
		OWLClassA merged = em.merge(detached);
		assertTrue(em.contains(merged));
		em.getTransaction().commit();

		final OWLClassA res = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(res);
		assertEquals(detached.getStringAttribute(), res.getStringAttribute());
	}

	@Test
	public void testMergeDetachedIntoContext() {
		LOG.config("Test: merge detached entity into context.");
		em = TestEnvironment.getPersistenceConnector("MergeDetachedIntoContext", storages, true);
		final List<Context> contexts = em.getAvailableContexts();
		em.getTransaction().begin();
		for (Context ctx : contexts) {
			em.persist(entityA, ctx.getUri());
		}
		em.getTransaction().commit();
		em.clear();

		final Context ctx = contexts.get(contexts.size() - 1);
		final OWLClassA detached = new OWLClassA();
		detached.setUri(entityA.getUri());
		detached.setTypes(entityA.getTypes());
		detached.setStringAttribute("SomeOtherStringAttribute");
		em.getTransaction().begin();
		em.merge(detached, ctx.getUri());
		em.getTransaction().commit();

		for (Context c : contexts) {
			final OWLClassA res = em.find(OWLClassA.class, entityA.getUri(), c.getUri());
			assertNotNull(res);
			if (c.getUri().equals(ctx.getUri())) {
				assertEquals(detached.getStringAttribute(), res.getStringAttribute());
			} else {
				assertEquals(entityA.getStringAttribute(), res.getStringAttribute());
			}
		}
	}

	@Test
	public void testMergeNew() {
		LOG.config("Test: merge new entity. Should behave like persist.");
		em = TestEnvironment.getPersistenceConnector("MergeNew", storages, false);
		em.getTransaction().begin();
		em.merge(entityA);
		em.getTransaction().commit();
		em.clear();

		final OWLClassA res = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(res);
	}

	@Test(expected = NullPointerException.class)
	public void testMergeNull() {
		LOG.config("Test: merge null.");
		em = TestEnvironment.getPersistenceConnector("MergeNull", storages, false);
		em.getTransaction().begin();
		em.merge(null);
		// Rollback is done by the tearDown method
		fail("This line should not have been reached.");
	}

	@Test(expected = OWLPersistenceException.class)
	public void testMergeUnknownContext() {
		LOG.config("Test: merge entity into unknown context.");
		em = TestEnvironment.getPersistenceConnector("MergeUnknownContext", storages, false);
		em.getTransaction().begin();
		final URI unknown = URI.create("http://www.unknown.org");
		em.merge(entityA, unknown);
		fail("This line should not have been reached.");
	}

	@Test
	public void testUpdateReference() {
		LOG.config("Test: update reference to another entity.");
		em = TestEnvironment.getPersistenceConnector("UpdateReference", storages, true);
		assertTrue(em.getAvailableContexts().size() > 2);
		final Context ctx = em.getAvailableContexts().get(2);
		em.getTransaction().begin();
		em.persist(entityA, ctx.getUri());
		em.persist(entityD, ctx.getUri());
		em.getTransaction().commit();

		final OWLClassD d = em.find(OWLClassD.class, entityD.getUri(), ctx.getUri());
		assertNotNull(d);
		assertNotNull(d.getOwlClassA());
		em.getTransaction().begin();
		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newA"));
		newA.setStringAttribute("NewAStringAttribute");
		d.setOwlClassA(newA);
		em.persist(newA, ctx.getUri());
		em.getTransaction().commit();
		em.clear();

		final OWLClassD resD = em.find(OWLClassD.class, entityD.getUri());
		assertNotNull(resD);
		assertNotNull(resD.getOwlClassA());
		assertEquals(newA.getUri(), resD.getOwlClassA().getUri());
		assertEquals(newA.getStringAttribute(), resD.getOwlClassA().getStringAttribute());
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		final OWLClassA newARes = em.find(OWLClassA.class, newA.getUri());
		assertNotNull(newARes);
		assertEquals(newARes, resD.getOwlClassA());
	}

	@Test
	public void testSetReferenceToNull() {
		LOG.config("Test: update, set reference to null.");
		em = TestEnvironment.getPersistenceConnector("SetReferenceToNull", storages, true);
		em.getTransaction().begin();
		em.persist(entityH);
		assertTrue(em.contains(entityH));
		assertTrue(em.contains(entityA));
		em.getTransaction().commit();

		final OWLClassH h = em.find(OWLClassH.class, entityH.getUri());
		assertNotNull(h);
		assertNotNull(h.getOwlClassA());
		em.getTransaction().begin();
		h.setOwlClassA(null);
		em.getTransaction().commit();

		final OWLClassH resH = em.find(OWLClassH.class, entityH.getUri());
		assertNotNull(resH);
		assertNull(resH.getOwlClassA());
		final OWLClassA resA = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(resA);
		assertNull(resH.getOwlClassA());
	}

	@Test
	public void testSetNewCollection() {
		LOG.config("Test: update, set new types collection.");
		em = TestEnvironment.getPersistenceConnector("SetNewCollection", storages, true);
		em.getTransaction().begin();
		em.persist(entityA);
		em.getTransaction().commit();

		final Set<String> newTypes = new HashSet<String>();
		newTypes.add(OWLClassE.class.getName());
		newTypes.add(OWLClassI.class.getName());
		newTypes.add(OWLClassH.class.getName());

		final OWLClassA a = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(a);
		assertNotNull(a.getTypes());
		assertFalse(a.getTypes().isEmpty());
		em.getTransaction().begin();
		a.setTypes(newTypes);
		em.getTransaction().commit();

		final OWLClassA res = em.find(OWLClassA.class, entityA.getUri());
		assertNotNull(res);
		assertNotNull(res.getTypes());
		assertEquals(newTypes.size(), res.getTypes().size());
		assertTrue(newTypes.containsAll(res.getTypes()));
	}

	@Test
	public void testUpdateSimpleList() {
		LOG.config("Test: add an entity into simple list.");
		em = TestEnvironment.getPersistenceConnector("UpdateSimpleList", storages, true);
		assertTrue(em.getAvailableContexts().size() > 1);
		final Context ctx = em.getAvailableContexts().get(0);
		em.getTransaction().begin();
		em.persist(entityA, ctx.getUri());
		em.persist(entityC, ctx.getUri());
		em.getTransaction().commit();

		final OWLClassA newA = new OWLClassA();
		newA.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/tests/newA"));
		newA.setStringAttribute("NewAStringAttribute");
		final OWLClassC c = em.find(OWLClassC.class, entityC.getUri(), ctx.getUri());
		assertNotNull(c);
		assertFalse(c.getSimpleList().isEmpty());
		em.getTransaction().begin();		
		c.getSimpleList().add(newA);
		em.persist(newA, ctx.getUri());
		em.getTransaction().commit();

		final OWLClassC resC = em.find(OWLClassC.class, entityC.getUri(), ctx.getUri());
		assertNotNull(resC);
		assertEquals(entityC.getSimpleList().size() + 1, resC.getSimpleList().size());
		boolean found = false;
		OWLClassA simpleA = null;
		for (OWLClassA a : resC.getSimpleList()) {
			if (newA.getUri().equals(a.getUri())) {
				found = true;
				simpleA = a;
				break;
			}
		}
		assertTrue(found);
		final OWLClassA resA = em.find(OWLClassA.class, newA.getUri(), ctx.getUri());
		assertNotNull(resA);
		assertEquals(simpleA, resA);
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
