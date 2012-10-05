package cz.cvut.kbss.jopa.owlapi.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.persistence.EntityTransaction;
import javax.persistence.RollbackException;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.OWLPersistentObjectException;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassC;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.OWLClassE;
import cz.cvut.kbss.jopa.owlapi.OWLClassF;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;

/**
 * These unit tests test persisting entities.
 * 
 * @author kidney
 * 
 */
public class TestCreateOperations {

	private Logger log = TestEnvironment.getLogger();

	private static OWLClassC testC;
	private static OWLClassF testF;
	private static List<OWLClassA> classes;
	private static List<OWLClassA> refList;

	private EntityManager pc;

	@BeforeClass
	public static void beforeClass() throws Exception {
		testC = new OWLClassC();
		final URI pkC = URI.create("http://testC");
		testC.setUri(pkC);
		classes = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			URI pkA = URI.create("http://classA" + Integer.toString(i));
			a.setUri(pkA);
			a.setStringAttribute("StringAttribute" + Integer.toString(i + 1));
			classes.add(a);
		}
		refList = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			final URI pkRA = URI
					.create("http://refA" + Integer.toString(i + 1));
			a.setUri(pkRA);
			a.setStringAttribute("strAttForRefA_" + Integer.toString(i + 1));
			refList.add(a);
		}
		testC.setSimpleList(classes);
		testF = new OWLClassF();
		final URI pkF = URI.create("http://testF");
		testF.setUri(pkF);
	}

	@After
	public void tearDown() throws Exception {
		if (pc.isOpen()) {
			pc.close();
			pc.getEntityManagerFactory().close();
		}
	}

	/**
	 * Just try to begin and commit a transaction. No operations are performed.
	 */
	@Test
	public void testBeginAndCommitTransaction() {
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testBeginAndCommit");
		pc.clear();
		try {
			EntityTransaction et = pc.getTransaction();
			et.begin();
			assertNotNull(et);
			et.commit();
		} catch (OWLPersistenceException e) {
			log.info("Transaction acquisition failed: " + e.getMessage());
		}
	}

	@Test
	public void testRollbackTransaction() {
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testRollback");
		pc.clear();
		final OWLClassB testEntity = new OWLClassB();
		final URI pk = URI.create("http://pk");
		testEntity.setUri(pk);
		try {
			pc.getTransaction().begin();
			pc.persist(testEntity);
			pc.getTransaction().rollback();
			assertFalse(pc.contains(testEntity));
		} catch (OWLPersistenceException e) {
			log.severe("Test failed.");
			e.printStackTrace();
			fail();
		}
	}

	@Test(expected = RollbackException.class)
	public void testSetRollbackOnly() {
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testRollbackOnly");
		pc.clear();
		final OWLClassA testEntity = new OWLClassA();
		final URI pk = URI.create("http://testPK");
		testEntity.setUri(pk);
		pc.getTransaction().begin();
		pc.persist(testEntity);
		pc.getTransaction().setRollbackOnly();
		pc.getTransaction().commit();
		fail("This line should not have been reached.");
	}

	@Test
	public void testPersistEntity() {
		log.info("Test: Persist entity");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testPersistEntity");
		pc.clear();
		final OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://testA");
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

	@Test
	public void testPersistEvictCache() {
		log.info("Test: persist entity. Clear cache before presence check.");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testPersistEvictCache");
		final OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://testA");
		entity.setUri(pk);
		entity.setStringAttribute("TEST");
		try {
			pc.getTransaction().begin();
			pc.persist(entity);
			pc.getTransaction().commit();
			pc.getEntityManagerFactory().getCache().evictAll();
			OWLClassA ent = pc.find(OWLClassA.class, pk);
			assertTrue(entity.getUri().equals(ent.getUri()));
		} catch (OWLPersistenceException e) {
			log.info("Persisting failed.");
			fail();
		}
	}

	@Test
	public void testPersistViolatingIC() {
		log.info("Test: persist Entity with IC violation");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testPersistViolatingIC");
		pc.clear();
		final OWLClassB entityOne = new OWLClassB();
		final URI pkOne = URI.create("http://testOne");
		entityOne.setUri(pkOne);
		final OWLClassB entityTwo = new OWLClassB();
		entityTwo.setUri(pkOne);
		entityTwo.setStringAttribute("testAttribute");
		try {
			pc.getTransaction().begin();
			pc.persist(entityTwo);
			pc.persist(entityOne);
			pc.getTransaction().commit();
			fail();
		} catch (OWLPersistenceException e) {
			log.severe("Exception raised! Right.");
			log.severe(e.getMessage() + "\n" + e.getCause().getMessage());
		}
	}

	@Test
	public void testPersistNull() {
		log.info("Test: persist a null object");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testPersistNull");
		pc.clear();
		final OWLClassA testEntity = new OWLClassA();
		try {
			pc.getTransaction().begin();
			pc.persist(testEntity);
			pc.getTransaction().commit();
			fail();
		} catch (OWLPersistenceException e) {
			log.severe("Exception raised! Right.");
			log.severe(e.getMessage());
		}
	}

	@Test
	public void testPersistEntityWithCascade() {
		log.info("Test: persistEntityWithCascade");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testPersistEntityWithCascade");
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
	public void testGenerateID() {
		log.info("Test: generate IRI for entity.");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testGenerateID");
		final OWLClassE entity = new OWLClassE();
		entity.setStringAttribute("someString");
		pc.getTransaction().begin();
		pc.persist(entity);
		final URI pk = entity.getUri();
		assertNotNull(pk);
		pc.getTransaction().commit();

		pc.clear();
		assertNotNull(pc.find(OWLClassE.class, pk));
	}

	@Test(expected = OWLPersistenceException.class)
	public void testGenerateIDNotGenerated() {
		log.info("Test: try generating IRI for entity which has not set the generated option.");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testGenerateIDNotGenerated");
		final OWLClassB entity = new OWLClassB();
		entity.setStringAttribute("stringAttribute");
		pc.getTransaction().begin();
		pc.persist(entity);
		pc.getTransaction().commit();
	}

	@Test
	public void testPersistSimpleList() {
		log.info("Test: persist an entity containing simple list of referenced entites");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testPersistWithSimpleList");
		pc.getTransaction().begin();
		for (OWLClassA ent : classes) {
			pc.persist(ent);
		}
		pc.persist(testC);
		pc.getTransaction().commit();
		final OWLClassC res = pc.find(OWLClassC.class, testC.getUri());
		assertNotNull(res);
		assertEquals(classes.size(), res.getSimpleList().size());
		assertNull(res.getReferencedList());
	}

	@Test
	public void testPersistEntityWithLists() {
		log.info("Test: persist entity with referenced list and simple list");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testPersistEntityWithLists");
		testC.setReferencedList(refList);
		pc.getTransaction().begin();
		for (OWLClassA ref : testC.getReferencedList()) {
			pc.persist(ref);
		}
		for (OWLClassA simple : testC.getSimpleList()) {
			pc.persist(simple);
		}
		pc.persist(testC);
		pc.getTransaction().commit();
		final OWLClassC c = pc.find(OWLClassC.class, testC.getUri());
		assertNotNull(c);
		assertEquals(refList.get(0).getStringAttribute(), c.getReferencedList()
				.get(0).getStringAttribute());
		assertEquals(classes.get(5).getUri(), c.getSimpleList().get(5).getUri());
	}

	@Test(expected = OWLPersistentObjectException.class)
	public void testPersistDetachedEntity() {
		log.info("Test: persist detached entity.");
		pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testPersistDetachedEntity");
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://testA");
		a.setUri(pk);
		a.setStringAttribute("stringAttribute");
		pc.getTransaction().begin();
		pc.persist(a);
		pc.getTransaction().commit();
		final OWLClassA det = pc.find(OWLClassA.class, pk);
		assertNotNull(det);
		pc.detach(det);
		pc.persist(det);
	}
}
