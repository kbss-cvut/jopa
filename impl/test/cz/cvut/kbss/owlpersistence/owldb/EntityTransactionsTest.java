package cz.cvut.kbss.owlpersistence.owldb;

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

import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.owlpersistence.model.EntityManager;
import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.model.OWLPersistentObjectException;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassA;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassB;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassC;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassD;
import cz.cvut.kbss.owlpersistence.owlapi.TestEnvironment;

public class EntityTransactionsTest {

	private static int index;
	private static EntityManager pc;

	private Logger log = TestEnvironment.getLogger();

	private OWLClassC testC;
	private OWLClassC testCWithRefs;
	private OWLClassC testCToChange;
	private List<OWLClassA> classes;
	private List<OWLClassA> simples;
	private List<OWLClassA> refList;

	public EntityTransactionsTest() {
		this.testC = new OWLClassC();
		final URI pkC = URI.create("http://testC");
		testC.setUri(pkC);
		this.testCWithRefs = new OWLClassC();
		final URI pkC2 = URI.create("http://testCWitRefs");
		testCWithRefs.setUri(pkC2);
		this.testCToChange = new OWLClassC();
		final URI pkC3 = URI.create("http://testCToChange");
		testCToChange.setUri(pkC3);
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
			a.setStringAttribute("StringAttributeSimple"
					+ Integer.toString(i + 1));
			this.simples.add(a);
		}
		this.refList = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			final URI pkRA = URI
					.create("http://refA" + Integer.toString(i + 1));
			a.setUri(pkRA);
			a.setStringAttribute("strAttForRefA_" + Integer.toString(i + 1));
			this.refList.add(a);
		}
		testCWithRefs.setSimpleList(simples);
		testCWithRefs.setReferencedList(refList);
		final List<OWLClassA> simple = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			URI pkA = URI
					.create("http://simpleToChangeA" + Integer.toString(i));
			a.setUri(pkA);
			a.setStringAttribute("StringAttributeSimple"
					+ Integer.toString(i + 1));
			simple.add(a);
		}
		final List<OWLClassA> refs = new ArrayList<OWLClassA>();
		for (int i = 0; i < 10; i++) {
			OWLClassA a = new OWLClassA();
			final URI pkRA = URI.create("http://refAToChange"
					+ Integer.toString(i + 1));
			a.setUri(pkRA);
			a.setStringAttribute("strAttForRefA_" + Integer.toString(i + 1));
			refs.add(a);
		}
		testCToChange.setSimpleList(simple);
		testCToChange.setReferencedList(refs);
	}

	@BeforeClass
	public static void setupBeforeClass() {
		index = 100;
		pc = TestEnvironment.getPersistenceConnector(
				"TestPersistenceConnectorLogic-testOWLDBTransactions", true,
				true);
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

	@Test(expected = OWLPersistenceException.class)
	public void testPersistViolatingIC() {
		log.info("Test: persist Entity with IC violation");
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
			fail("This line should not have been reached.");
		} catch (OWLPersistenceException e) {
			log.info("Exception caught. Correct.");
			pc.getTransaction().rollback();
			throw e;
		}
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
		assertEquals(refList.get(0).getStringAttribute(), c.getReferencedList()
				.get(0).getStringAttribute());
		assertEquals(simples.get(5).getUri(), c.getSimpleList().get(5).getUri());
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

	@Test(expected = OWLPersistentObjectException.class)
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
