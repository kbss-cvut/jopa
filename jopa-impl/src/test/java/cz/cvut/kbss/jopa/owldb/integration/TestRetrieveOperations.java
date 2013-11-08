package cz.cvut.kbss.jopa.owldb.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.logging.Logger;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public class TestRetrieveOperations {

	private static int index;
	private static EntityManager pc;

	private Logger log = TestEnvironment.getLogger();

	public TestRetrieveOperations() {

	}

	@BeforeClass
	public static void setupBeforeClass() throws Exception {
		index = 1;
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
		pc = TestEnvironment.getPersistenceConnector("OWLDBPersistenceTest-retrieve",
				OwlapiStorageType.OWLDB, true);
	}

	@AfterClass
	public static void teardownAfterClass() {
		pc.getEntityManagerFactory().close();
	}

	@Test
	public void testFindEntity() {
		log.info("Test: Find entity");
		pc.clear();
		final OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://testA" + (index++));
		entity.setUri(pk);
		try {
			pc.getTransaction().begin();
			pc.persist(entity);
			pc.getTransaction().commit();
			pc.clear();
			OWLClassA result = pc.find(OWLClassA.class, pk);
			assertNotNull(result);
			assertEquals(pk, result.getUri());
		} catch (OWLPersistenceException e) {
			log.info("Loading entity failed.");
			fail();
		}
	}

	@Test
	public void testContainsMember() {
		log.info("Test: does the EntityManager contain object which is a member of another object");
		pc.clear();
		final OWLClassA a = new OWLClassA();
		final URI pkOne = URI.create("http://testA" + (index++));
		a.setUri(pkOne);
		a.setStringAttribute("someStringAttribute");
		final OWLClassD d = new OWLClassD();
		final URI pkTwo = URI.create("http://testD");
		d.setUri(pkTwo);
		d.setOwlClassA(a);
		pc.getTransaction().begin();
		pc.persist(a);
		pc.persist(d);
		pc.getTransaction().commit();
		OWLClassD resD = pc.find(OWLClassD.class, pkTwo);
		assertNotNull(resD);
		assertTrue(pc.contains(resD.getOwlClassA()));
	}

	@Test
	public void testRefreshEntity() {
		log.info("Test: refresh the entity state.");
		pc.clear();
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://testA" + (index++));
		a.setUri(pk);
		a.setStringAttribute("stringAttribute");
		pc.getTransaction().begin();
		pc.persist(a);
		pc.getTransaction().commit();
		OWLClassA toChange = pc.find(OWLClassA.class, pk);
		assertNotNull(toChange);
		toChange.setStringAttribute("newString");
		pc.refresh(toChange);
		assertEquals(a.getStringAttribute(), toChange.getStringAttribute());
	}

}
