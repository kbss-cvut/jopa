package cz.cvut.kbss.jopa.owlapi_vs_owldb;

import static org.junit.Assert.assertNotNull;

import java.net.URI;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public class FileOWLDBPerformanceTest {

	private static final Logger LOG = Logger.getLogger(FileOWLDBPerformanceTest.class.getName());
	private static final int COUNT = 1000;
	private static final int FIND_CNT = COUNT / 10;
	private static final String IRI_PREFIX = "http://classA";

	private static boolean shouldDropDb = true;

	private static List<OWLClassA> entitiesA;
	private static List<URI> ids;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		entitiesA = new ArrayList<OWLClassA>(COUNT);
		for (int i = 0; i < COUNT; i++) {
			final OWLClassA a = new OWLClassA();
			final URI pk = URI.create(IRI_PREFIX + Integer.toString(i));
			final String strAtt = "StringAttribute";
			a.setUri(pk);
			a.setStringAttribute(strAtt);
			final Set<String> types = new HashSet<String>();
			types.add("TypeOne");
			types.add("TypeTwo");
			types.add("TypeThree");
			a.setTypes(types);
			entitiesA.add(a);
		}
		final Random rand = new Random();
		ids = new ArrayList<URI>();
		for (int i = 0; i < FIND_CNT; i++) {
			final Integer id = rand.nextInt(COUNT);
			ids.add(URI.create(IRI_PREFIX + id));
		}
	}

	@Before
	public void setUp() throws Exception {
		if (shouldDropDb) {
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
			shouldDropDb = false;
		}
	}

	@Test
	public void testFileOntologyPerformancePersist() {
		LOG.config("Testing file ontology access performance. Persisting " + COUNT + " entities.");
		final EntityManager em = TestEnvironment.getPersistenceConnector(
				"FileOntologyPerformanceTest-Persist", OwlapiStorageType.FILE, true);

		persistEntities(em);
	}

	@Test
	public void testOWLDBOntologyPerformancePersist() {
		LOG.config("Testing OWLDB ontology access performance. Persisting " + COUNT + " entities.");
		final EntityManager em = TestEnvironment.getPersistenceConnector(
				"OWLDBOntologyPerformanceTest-Persist", OwlapiStorageType.OWLDB, true);
		try {
			persistEntities(em);
		} finally {
			em.getEntityManagerFactory().close();
		}
		shouldDropDb = true;
	}

	/**
	 * Persist predefined entities using the specified entity manager.
	 * 
	 * @param em
	 *            EntityManager
	 */
	private void persistEntities(final EntityManager em) {
		em.getTransaction().begin();
		for (OWLClassA a : entitiesA) {
			em.persist(a);
		}
		em.getTransaction().commit();
	}

	@Test
	public void testFileOntologyPerformanceRead() {
		LOG.config("Search for several randomly chosen entities and measure file ontology performance.");
		final EntityManager em = TestEnvironment.getPersistenceConnector(
				"FileOntologyPerformanceTest-Find", OwlapiStorageType.FILE, false);
		persistEntities(em);

		findEntities(em);
	}

	@Test
	public void testOWLDBOntologyPerformanceRead() {
		LOG.config("Search for several randomly chosen entities and measure OWLDB ontology performance.");
		final EntityManager em = TestEnvironment.getPersistenceConnector(
				"OWLDBOntologyPerformanceTest-Find", OwlapiStorageType.OWLDB, false);
		try {
			persistEntities(em);

			findEntities(em);
		} finally {
			em.close();
			em.getEntityManagerFactory().close();
		}
		shouldDropDb = true;
	}

	private void findEntities(final EntityManager em) {
		em.clear();
		for (URI id : ids) {
			final OWLClassA a = em.find(OWLClassA.class, id);
			assertNotNull(a);
		}
	}

}
