package cz.cvut.kbss.jopa.test.owldb.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.Collections;
import java.util.logging.Logger;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.jopa.test.utils.OwldbStorageConfig;
import cz.cvut.kbss.jopa.test.utils.StorageConfig;

public class TestRetrieveOperations {

	private static int index;
	private static EntityManager pc;

	private Logger log = TestEnvironment.getLogger();

	public TestRetrieveOperations() {

	}

	@BeforeClass
	public static void setupBeforeClass() throws Exception {
		index = 1;
		TestEnvironment.clearDatabase();
		TestEnvironment.resetOwldbHibernateProvider();
		pc = TestEnvironment.getPersistenceConnector("OWLDBPersistenceTest-retrieve",
				Collections.<StorageConfig> singletonList(new OwldbStorageConfig()), true);
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
