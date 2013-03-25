package cz.cvut.kbss.jopa.owldb.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassC;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiStorageType;

public class TestUpdateOperations {

	private static int index;
	private static EntityManager pc;

	private Logger log = TestEnvironment.getLogger();

	private OWLClassC testCToChange;

	public TestUpdateOperations() {
		this.testCToChange = new OWLClassC();
		final URI pkC3 = URI.create("http://testCToChange");
		testCToChange.setUri(pkC3);
		final List<OWLClassA> simple = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			URI pkA = URI.create("http://simpleToChangeA" + Integer.toString(i));
			a.setUri(pkA);
			a.setStringAttribute("StringAttributeSimple" + Integer.toString(i + 1));
			simple.add(a);
		}
		final List<OWLClassA> refs = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			final URI pkRA = URI.create("http://refAToChange" + Integer.toString(i + 1));
			a.setUri(pkRA);
			a.setStringAttribute("strAttForRefA_" + Integer.toString(i + 1));
			refs.add(a);
		}
		testCToChange.setSimpleList(simple);
		testCToChange.setReferencedList(refs);
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
		pc = TestEnvironment.getPersistenceConnector("OWLDBPersistenceTest-update",
				OwlapiStorageType.OWLDB, true);
	}

	@AfterClass
	public static void teardownAfterClass() {
		pc.getEntityManagerFactory().close();
	}

	@After
	public void tearDown() throws Exception {
		if (pc.getTransaction().isActive()) {
			pc.getTransaction().rollback();
		}
	}

	@Test
	public void testMergeDetachedEntity() {
		log.info("Test: Merge detached (detached during transaction)");
		pc.clear();
		OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://testA" + (index++));
		entity.setStringAttribute("OriginalStringAttribute");
		entity.setUri(pk);
		pc.getTransaction().begin();
		pc.persist(entity);
		pc.getTransaction().commit();
		pc.getTransaction().begin();
		OWLClassA det = pc.find(OWLClassA.class, pk);
		pc.detach(det);
		assertFalse(pc.contains(det));
		det.setStringAttribute("NewStringAttribute");
		pc.merge(det);
		assertTrue(pc.contains(det));
		pc.getTransaction().commit();
		det = pc.find(OWLClassA.class, pk);
		assertNotNull(det);
		assertEquals("NewStringAttribute", det.getStringAttribute());
	}

	@Test
	public void testMergeDetachedOutsideTransaction() {
		log.info("Test: Merge detached (detached outside transaction)");
		pc.clear();
		OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://testA" + (index++));
		entity.setStringAttribute("OriginalStringAttribute");
		entity.setUri(pk);
		pc.getTransaction().begin();
		pc.persist(entity);
		pc.getTransaction().commit();
		OWLClassA det = pc.find(OWLClassA.class, pk);
		pc.detach(det);
		det.setStringAttribute("NewStringAttribute");
		pc.getTransaction().begin();
		pc.merge(det);
		pc.getTransaction().commit();
		det = pc.find(OWLClassA.class, pk);
		assertNotNull(det);
		assertEquals("NewStringAttribute", det.getStringAttribute());
	}

	@Test
	public void testChangeReferenceInList() {
		log.info("Test: change a reference in referenced list of an entity and persist this change");
		pc.clear();
		pc.getTransaction().begin();
		for (OWLClassA ref : testCToChange.getReferencedList()) {
			pc.persist(ref);
		}
		for (OWLClassA simple : testCToChange.getSimpleList()) {
			pc.persist(simple);
		}
		pc.persist(testCToChange);
		pc.getTransaction().commit();
		OWLClassC c = pc.find(OWLClassC.class, testCToChange.getUri());
		assertNotNull(c);
		pc.getTransaction().begin();
		OWLClassA a = c.getReferencedList().get(3);
		final String nStr = "newString";
		a.setStringAttribute(nStr);
		pc.getTransaction().commit();
		c = pc.find(OWLClassC.class, testCToChange.getUri());
		boolean found = false;
		for (OWLClassA aa : c.getReferencedList()) {
			if (nStr.equals(aa.getStringAttribute())) {
				found = true;
			}
		}
		assertTrue(found);
	}
}
