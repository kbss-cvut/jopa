package cz.cvut.kbss.owlpersistence.owlapi.transactionsUnitTests;

import static org.junit.Assert.*;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import javax.persistence.EntityTransaction;

import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.OWLPersistentObjectException;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassA;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassB;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassC;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassD;
import cz.cvut.kbss.owlpersistence.owlapi.TestEnvironment;

public class TestEntityTransactions {

	private Logger log = TestEnvironment.getLogger();

	private OWLClassC testC;
	private List<OWLClassA> classes;
	private List<OWLClassA> refList;

	public TestEntityTransactions() {
		this.testC = new OWLClassC();
		final URI pkC = URI.create("http://testC");
		this.testC.setUri(pkC);
		this.classes = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			URI pkA = URI.create("http://classA" + Integer.toString(i));
			a.setUri(pkA);
			a.setStringAttribute("StringAttribute" + Integer.toString(i + 1));
			this.classes.add(a);
		}
		this.refList = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			final URI pkRA = URI.create("http://refA" + Integer.toString(i + 1));
			a.setUri(pkRA);
			a.setStringAttribute("strAttForRefA_" + Integer.toString(i + 1));
			this.refList.add(a);
		}
		testC.setSimpleList(classes);
	}

	/**
	 * Just try to begin and commit a transaction. No operations are performed.
	 */
	@Test
	public void testBeginAndCommitTransaction() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testBeginAndCommit");
		pc.clear();
		try {
			EntityTransaction et = pc.getTransaction();
			et.begin();
			assertNotNull(et);
			et.commit();
		} catch (OWLPersistenceException e) {
			log.info("Transaction acquisition failed: " + e.getMessage());
		} finally {
			pc.close();
		}
	}

	@Test
	public void testRollbackTransaction() {
		EntityManager pc = TestEnvironment
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
		} finally {
			pc.close();
		}
	}

	@Test
	public void testSetRollbackOnly() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testRollbackOnly");
		pc.clear();
		final OWLClassA testEntity = new OWLClassA();
		final URI pk = URI.create("http://testPK");
		testEntity.setUri(pk);
		try {
			pc.getTransaction().begin();
			pc.persist(testEntity);
			pc.getTransaction().setRollbackOnly();
			pc.getTransaction().commit();
			assertFalse(pc.contains(testEntity));
		} catch (OWLPersistenceException e) {
			e.printStackTrace();
			log.severe(e.getMessage());
			fail();
		} finally {
			pc.close();
		}
	}

	@Test
	public void testPersistEntity() {
		log.info("Test: Persist entity");
		EntityManager pc = TestEnvironment
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
		} finally {
			pc.close();
		}
	}

	@Test
	public void testPersistViolatingIC() {
		log.info("Test: persist Entity with IC violation");
		EntityManager pc = TestEnvironment
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
		} finally {
			pc.close();
		}
	}

	@Test
	public void testPersistNull() {
		log.info("Test: persist a null object");
		EntityManager pc = TestEnvironment
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
		} finally {
			pc.close();
		}
	}

	@Test
	public void testPersistEntityWithCascade() {
		log.info("Test: persistEntityWithCascade");
		EntityManager pc = TestEnvironment
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
		} finally {
			pc.close();
		}
	}

	@Test
	public void testFindEntity() {
		log.info("Test: Find entity");
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testFindEntity");
		pc.clear();
		final OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://textA");
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
		} finally {
			pc.close();
		}
	}

	@Test
	public void testFindNull() {
		log.info("Test: Find null entity");
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testFindNull");
		pc.clear();
		final OWLClassA res;
		final URI uri = null;
		try {
			res = pc.find(OWLClassA.class, uri);
			assertNull(res);
		} catch (OWLPersistenceException e) {
			e.printStackTrace();
			fail();
		} finally {
			pc.close();
		}
	}

	@Test
	public void testRemoveEntity() {
		log.info("Test: Remove entity");
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testRemoveEntity");
		pc.clear();
		OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://textA");
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
		} finally {
			pc.close();
		}
	}

	@Test
	public void testMergeDetachedEntity() {
		log.info("Test: Merge detached (detached during transaction)");
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testMergeDetachedEntity");
		OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://TestA");
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
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testMergeDetachedOutsideTransaction");
		OWLClassA entity = new OWLClassA();
		final URI pk = URI.create("http://TestA");
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
	public void testPersistSimpleList() {
		log.info("Test: persist an entity containing simple list of referenced entites");
		EntityManager pc = TestEnvironment
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
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testPersistEntityWithLists");
		this.testC.setReferencedList(refList);
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
		assertEquals(refList.get(0).getStringAttribute(), c.getReferencedList().get(0).getStringAttribute());
		assertEquals(classes.get(5).getUri(), c.getSimpleList().get(5).getUri());
	}
	
	@Test
	public void testChangeReferenceInList() {
		log.info("Test: change a reference in referenced list of an entity and persist this change");
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testChangeReferenceInList");
		this.testC.setReferencedList(refList);
		pc.getTransaction().begin();
		for (OWLClassA ref : testC.getReferencedList()) {
			pc.persist(ref);
		}
		for (OWLClassA simple : testC.getSimpleList()) {
			pc.persist(simple);
		}
		pc.persist(testC);
		pc.getTransaction().commit();
		OWLClassC c = pc.find(OWLClassC.class, testC.getUri());
		assertNotNull(c);
		pc.getTransaction().begin();
		OWLClassA a = c.getReferencedList().get(3);
		final String nStr = "newString";
		a.setStringAttribute(nStr);
		pc.getTransaction().commit();
		c = pc.find(OWLClassC.class, testC.getUri());
		boolean found = false;
		for (OWLClassA aa : c.getReferencedList()) {
			if (nStr.equals(aa.getStringAttribute())) {
				found = true;
			}
		}
		assertTrue(found);
	}

	@Test
	public void testContainsMember() {
		log.info("Test: does the EntityManager contain object which is a member of another object");
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testContainsMember");
		final OWLClassA a = new OWLClassA();
		final URI pkOne = URI.create("http://testA");
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
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testRefreshEntity");
		final OWLClassA a = new OWLClassA();
		final URI pk = URI.create("http://testA");
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

	@Test(expected = OWLPersistentObjectException.class)
	public void testPersistDetachedEntity() {
		log.info("Test: persist detached entity.");
		EntityManager pc = TestEnvironment
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
