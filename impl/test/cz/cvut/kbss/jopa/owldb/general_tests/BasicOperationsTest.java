package cz.cvut.kbss.jopa.owldb.general_tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment.Storage;

public class BasicOperationsTest {

	private static final String dbUrl = "jdbc:postgresql://localhost/owldb";
	private static final String username = "owldb";
	private static final String password = "owldb";

	private static EntityManager em;

	@Before
	public void setup() throws Exception {
		Connection con = null;
		Statement st1 = null;
		Statement st2 = null;
		ResultSet rs = null;
		con = DriverManager.getConnection(dbUrl, username, password);
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
	}

	@After
	public void tearDown() throws Exception {
		if (em != null) {
			em.getEntityManagerFactory().close();
		}
	}

	@Test
	public void testFetchSimpleData() {
		em = TestEnvironment.getPersistenceConnector(
				"OWLDBTestBasicOperations-testFetchSimpleData", Storage.OWLDB,
				true);

		OWLClassA a = new OWLClassA();
		URI uri = URI.create("http://new#A");
		a.setUri(uri);

		a.setStringAttribute("new-value");

		em.getTransaction().begin();
		em.persist(a);
		assertTrue(em.contains(a));
		em.getTransaction().commit();

		em.clear();

		assertFalse(em.contains(a));

		final OWLClassA aX = em.find(OWLClassA.class, uri);

		assertNotNull(aX);

		assertEquals(aX.getStringAttribute(), "new-value");
	}

	@Test
	public void testPersistEntity() {
		em = TestEnvironment.getPersistenceConnector(
				"OWLDBTestBasicOperations-testPersistEntity", Storage.OWLDB,
				true);
		final OWLClassA a = new OWLClassA();
		final URI uri = URI.create("persistA");
		a.setUri(uri);
		final String str = "PersistTestString";
		a.setStringAttribute(str);
		em.getTransaction().begin();
		em.persist(a);
		em.getTransaction().commit();

		em.clear();

		final OWLClassA aX = em.find(OWLClassA.class, uri);
		assertNotNull(aX);
		assertEquals(str, aX.getStringAttribute());
	}

	@Test
	public void testPersistRelationship() {
		em = TestEnvironment.getPersistenceConnector(
				"OWLDBTestBasicOperations-testPersistRelationship",
				Storage.OWLDB, true);
		final OWLClassA a = new OWLClassA();
		final URI uri = URI.create("persistA");
		a.setUri(uri);
		final String str = "PersistTestString";
		a.setStringAttribute(str);
		final OWLClassD d = new OWLClassD();
		d.setOwlClassA(a);
		final URI dUri = URI.create("persistD");
		d.setUri(dUri);
		em.getTransaction().begin();
		em.persist(a);
		em.persist(d);
		em.getTransaction().commit();
		em.clear();

		final OWLClassD dX = em.find(OWLClassD.class, dUri);
		assertNotNull(dX);
		assertNotNull(dX.getOwlClassA());
		assertEquals(uri, dX.getOwlClassA().getUri());
	}

	@Test
	public void testRemoveEntity() {
		em = TestEnvironment.getPersistenceConnector(
				"OWLDBTestBasicOperations-testRemoveEntity", Storage.OWLDB,
				true);
		final OWLClassA a = new OWLClassA();
		final URI uri = URI.create("persistA");
		a.setUri(uri);
		em.getTransaction().begin();
		em.persist(a);
		em.getTransaction().commit();

		em.clear();
		assertFalse(em.contains(a));

		final OWLClassA aX = em.find(OWLClassA.class, uri);
		assertNotNull(aX);
		em.getTransaction().begin();
		em.remove(aX);
		em.getTransaction().commit();

		em.clear();
		final OWLClassA res = em.find(OWLClassA.class, uri);
		assertNull(res);
	}

}
