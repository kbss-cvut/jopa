package cz.cvut.kbss.jopa.test.owldb.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.utils.StorageType;

public class TestCreateOperations {

	private static int index;
	private static EntityManager pc;

	private Logger log = TestEnvironment.getLogger();

	private OWLClassC testC;
	private OWLClassC testCWithRefs;
	private List<OWLClassA> classes;
	private List<OWLClassA> simples;
	private List<OWLClassA> refList;

	public TestCreateOperations() {
		this.testC = new OWLClassC();
		final URI pkC = URI.create("http://testC");
		testC.setUri(pkC);
		this.testCWithRefs = new OWLClassC();
		final URI pkC2 = URI.create("http://testCWitRefs");
		testCWithRefs.setUri(pkC2);
		this.classes = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			URI pkA = URI.create("http://classA" + Integer.toString(i));
			a.setUri(pkA);
			a.setStringAttribute("StringAttribute" + Integer.toString(i + 1));
			this.classes.add(a);
		}
		testC.setSimpleList(classes);
		this.simples = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			URI pkA = URI.create("http://simpleA" + Integer.toString(i));
			a.setUri(pkA);
			a.setStringAttribute("StringAttributeSimple" + Integer.toString(i + 1));
			this.simples.add(a);
		}
		this.refList = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			final URI pkRA = URI.create("http://refA" + Integer.toString(i + 1));
			a.setUri(pkRA);
			a.setStringAttribute("strAttForRefA_" + Integer.toString(i + 1));
			this.refList.add(a);
		}
		testCWithRefs.setSimpleList(simples);
		testCWithRefs.setReferencedList(refList);
	}

	@BeforeClass
	public static void setupBeforeClass() throws Exception {
		index = 100;
		Connection con = null;
		Statement st1 = null;
		Statement st2 = null;
		ResultSet rs = null;
		con = DriverManager.getConnection(TestEnvironment.DB_URI, TestEnvironment.DB_USERNAME,
				TestEnvironment.DB_PASSWORD);
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
		TestEnvironment.resetOwldbHibernateProvider();
		pc = TestEnvironment.getPersistenceConnector("OWLDBPersistenceTest-create",
				StorageType.OWLDB, true);
	}

	@After
	public void tearDown() throws Exception {
		if (pc.getTransaction().isActive()) {
			pc.getTransaction().rollback();
		}
	}

	@AfterClass
	public static void teardownAfterClass() {
		pc.getEntityManagerFactory().close();
	}

	@Test
	public void testPersistEntity() {
		log.info("Test: Persist entity");
		pc.clear();
		final OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://testA" + (index++));
		entity.setUri(pk);
		entity.setStringAttribute("TEST");
		try {
			pc.getTransaction().begin();
			pc.persist(entity);
			pc.getTransaction().commit();
			OWLClassA ent = pc.find(OWLClassA.class, pk);
			assertTrue(entity.getUri().equals(ent.getUri()));
		} catch (OWLPersistenceException e) {
			log.info("Persisting failed.");
			fail();
		}
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistViolatingIC() {
		log.info("Test: persist Entity with IC violation");
		pc.clear();
		final OWLClassB entityOne = new OWLClassB();
		final URI pkOne = URI.create("http://testOne");
		entityOne.setUri(pkOne);
		final OWLClassB entityTwo = new OWLClassB();
		entityTwo.setUri(pkOne);
		entityTwo.setStringAttribute("testAttribute");
		pc.getTransaction().begin();
		pc.persist(entityTwo);
		pc.persist(entityOne);
		pc.getTransaction().commit();
		fail("This line should not have been reached.");
	}

	@Test(expected = OWLPersistenceException.class)
	public void testPersistNull() {
		log.info("Test: persist a null object");
		pc.clear();
		final OWLClassA testEntity = new OWLClassA();
		try {
			pc.getTransaction().begin();
			pc.persist(testEntity);
			pc.getTransaction().commit();
			fail();
		} catch (OWLPersistenceException e) {
			log.severe("Exception caught. Correct.");
			pc.getTransaction().rollback();
			throw e;
		}
	}

	@Test
	public void testPersistEntityWithCascade() {
		log.info("Test: persistEntityWithCascade");
		pc.clear();
		final OWLClassD testEntity = new OWLClassD();
		final URI pk = URI.create("http://testEntityURI");
		final OWLClassA referencedEntity = new OWLClassA();
		final URI refPK = URI.create("http://referencedEntityURI");
		testEntity.setUri(pk);
		referencedEntity.setUri(refPK);
		referencedEntity.setStringAttribute("testStringAttribute");
		testEntity.setOwlClassA(referencedEntity);
		try {
			pc.getTransaction().begin();
			// Since OWLClassD has not CASCADE set, we need to persist the
			// referenced
			// entity as well
			pc.persist(referencedEntity);
			pc.persist(testEntity);
			pc.getTransaction().commit();
			assertNotNull(pc.find(OWLClassD.class, pk));
		} catch (OWLPersistenceException e) {
			log.info("Persist failed");
			e.printStackTrace();
			fail();
		}
	}

	@Test
	public void testPersistSimpleList() {
		log.info("Test: persist an entity containing simple list of referenced entites");
		pc.clear();
		pc.getTransaction().begin();
		for (OWLClassA ent : classes) {
			pc.persist(ent);
		}
		pc.persist(testC);
		pc.getTransaction().commit();
		final OWLClassC res = pc.find(OWLClassC.class, testC.getUri());
		assertNotNull(res);
		assertEquals(testC.getSimpleList().size(), res.getSimpleList().size());
		assertNull(res.getReferencedList());
	}

	@Test
	public void testPersistEntityWithLists() {
		log.info("Test: persist entity with referenced list and simple list");
		pc.clear();
		pc.getTransaction().begin();
		for (OWLClassA ref : testCWithRefs.getReferencedList()) {
			pc.persist(ref);
		}
		for (OWLClassA simple : testCWithRefs.getSimpleList()) {
			pc.persist(simple);
		}
		pc.persist(testCWithRefs);
		pc.getTransaction().commit();
		final OWLClassC c = pc.find(OWLClassC.class, testCWithRefs.getUri());
		assertNotNull(c);
		assertEquals(refList.get(0).getStringAttribute(), c.getReferencedList().get(0)
				.getStringAttribute());
		assertEquals(simples.get(5).getUri(), c.getSimpleList().get(5).getUri());
	}

	@Test(expected = OWLEntityExistsException.class)
	public void testPersistDetachedEntity() {
		log.info("Test: persist detached entity.");
		pc.clear();
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://testA" + (index++));
		a.setUri(pk);
		a.setStringAttribute("stringAttribute");
		pc.getTransaction().begin();
		pc.persist(a);
		pc.getTransaction().commit();
		final OWLClassA det = pc.find(OWLClassA.class, pk);
		assertNotNull(det);
		pc.detach(det);
		pc.persist(det);
		fail("This line should not have been reached.");
	}

}
