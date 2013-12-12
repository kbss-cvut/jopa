package cz.cvut.kbss.jopa.test.owldb.integration;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
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
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.utils.StorageType;

public class TestDeleteOperations {

	private static int index;
	private static EntityManager pc;

	private Logger log = TestEnvironment.getLogger();

	public TestDeleteOperations() {
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
		TestEnvironment.resetOwldbHibernateProvider();
		pc = TestEnvironment.getPersistenceConnector("OWLDBPersistenceTest-delete",
				StorageType.OWLDB, true);
	}

	@AfterClass
	public static void teardownAfterClass() {
		pc.getEntityManagerFactory().close();
	}

	@Test
	public void testRemoveEntity() {
		log.info("Test: Remove entity");
		pc.clear();
		OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://testA" + (index++));
		entity.setUri(pk);
		try {
			pc.getTransaction().begin();
			pc.persist(entity);
			pc.getTransaction().commit();
			OWLClassA fnd = pc.find(OWLClassA.class, pk);
			assertNotNull(fnd);

			pc.getTransaction().begin();
			pc.remove(fnd);
			pc.getTransaction().commit();
			fnd = pc.find(OWLClassA.class, pk);
			assertNull(fnd);
		} catch (OWLPersistenceException e) {
			log.info("Remove failed with exception.");
			e.printStackTrace();
			fail();
		}
	}

}
