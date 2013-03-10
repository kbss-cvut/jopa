package cz.cvut.kbss.jopa.owlapi.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Vector;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.owlapi.EntityManagerImpl;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassB;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;
import cz.cvut.kbss.jopa.owlapi.EntityManagerImpl.State;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class UnitOfWorkTest {

	UnitOfWorkImpl testUOW;
	EntityManagerImpl em;
	OWLClassA testObject;
	OWLClassB testObjectTwo;
	OWLClassD testObjectThree;

	@Before
	public void setUp() {
		this.em = (EntityManagerImpl) TestEnvironment
				.getPersistenceConnector("UnitOfWorkJUnitTest");
		this.testUOW = this.em.getCurrentPersistenceContext();
		final URI pkOne = URI.create("http://testOne");
		this.testObject = new OWLClassA();
		this.testObject.setUri(pkOne);
		this.testObject.setStringAttribute("attribute");
		this.testObject.setTypes(new HashSet<String>());
		final URI pkTwo = URI.create("http://testTwo");
		this.testObjectTwo = new OWLClassB();
		this.testObjectTwo.setUri(pkTwo);
		final URI pkThree = URI.create("http://testThree");
		this.testObjectThree = new OWLClassD();
		this.testObjectThree.setUri(pkThree);
		this.testObjectThree.setOwlClassA(testObject);
		this.em.getTransaction().begin();
		this.em.persist(testObject);
		this.em.persist(testObjectTwo);
		this.em.persist(testObjectThree);
		this.em.getTransaction().commit();
		this.em.find(OWLClassA.class, pkOne);
		this.em.find(OWLClassB.class, pkTwo);
		this.em.find(OWLClassD.class, pkThree);
	}

	@After
	public void tearDown() {
		em.close();
	}

	@Test
	public void testReadObjectFromCache() {
		OWLClassA res = testUOW.readObject(OWLClassA.class, IRI.create(testObject.getUri()));
		assertNotNull(res);
		assertEquals(res.getStringAttribute(), testObject.getStringAttribute());
	}

	@Test(expected = NullPointerException.class)
	public void testReadObectNullPrimaryKey() {
		@SuppressWarnings("unused")
		final Object res = testUOW.readObject(testObject.getClass(), null);
		fail("This line should not have been reached.");
	}

	@Test(expected = NullPointerException.class)
	public void testReadObjectNullClass() {
		@SuppressWarnings("unused")
		final Object res = testUOW.readObject(null, testObjectTwo.getUri());
		fail("This line should not have been reached.");
	}

	@Test(expected = NullPointerException.class)
	public void testReadObjectNullContext() {
		@SuppressWarnings("unused")
		final Object res = testUOW.readObject(testObject.getClass(), testObject.getUri(), null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testReadObjectFromOntology() {
		// Clear the cache so the entity will be loaded from the ontology
		this.em.clear();
		OWLClassA res = this.testUOW.readObject(OWLClassA.class, IRI.create(testObject.getUri()));
		assertNotNull(res);
		assertEquals(testObject.getUri(), res.getUri());
	}

	@Test
	public void testCalculateChanges() {
		this.em.getTransaction().begin();
		OWLClassB toDelete = em.find(OWLClassB.class, IRI.create(testObjectTwo.getUri()));
		assertNotNull(toDelete);
		this.em.remove(toDelete);
		this.em.getTransaction().commit();
		OWLClassB res = em.find(OWLClassB.class, IRI.create(testObjectTwo.getUri()));
		assertNull(res);
	}

	@Test
	public void testCalculateNewObjects() {
		OWLClassB newOne = new OWLClassB();
		final URI pk = URI.create("http://testNewOne");
		newOne.setUri(pk);
		newOne.setStringAttribute("testAttributeOne");
		OWLClassB newTwo = new OWLClassB();
		final URI pkTwo = URI.create("http://testNewTwo");
		newTwo.setUri(pkTwo);
		this.em.getTransaction().begin();
		this.em.persist(newOne);
		this.em.persist(newTwo);
		this.em.getTransaction().commit();
		newOne = this.em.find(OWLClassB.class, newOne.getUri());
		newTwo = this.em.find(OWLClassB.class, newTwo.getUri());
		assertTrue(this.testUOW.isObjectManaged(newOne));
		assertTrue(this.testUOW.isObjectManaged(newTwo));
	}

	@Test
	public void testContains() {
		Object tO = this.em.find(testObject.getClass(), testObject.getUri());
		assertTrue(testUOW.contains(tO));
	}

	@Test
	public void testGetState() {
		assertEquals(State.DETACHED, testUOW.getState(testObject));
		Object toRemove = em.find(testObjectTwo.getClass(), testObjectTwo.getUri());
		testUOW.removeObject(toRemove);
		assertEquals(State.REMOVED, testUOW.getState(toRemove));
		final OWLClassA stateTest = new OWLClassA();
		final URI pk = URI.create("http://stateTest");
		stateTest.setUri(pk);
		assertEquals(State.NEW, testUOW.getState(stateTest));
	}

	@Test
	public void testGetOriginal() {
		Object tO = this.em.find(testObject.getClass(), testObject.getUri());
		OWLClassA origOne = (OWLClassA) testUOW.getOriginal(tO);
		assertEquals(origOne.getUri(), testObject.getUri());
		OWLClassA origTwo = (OWLClassA) testUOW.getOriginal(tO);
		assertEquals(origOne, origTwo);
	}

	@Test
	public void testGetOriginalNull() {
		assertNull(testUOW.getOriginal(null));
	}

	@Test
	public void testIsObjectNew() {
		final OWLClassA testNew = new OWLClassA();
		final URI pk = URI.create("http://testNewOne");
		testNew.setUri(pk);
		testUOW.registerNewObject(testNew);
		assertTrue(testUOW.isObjectNew(testNew));
	}

	@Test
	public void testIsObjectNewWithNullAndManaged() {
		assertFalse(testUOW.isObjectNew(null));
		assertFalse(testUOW.isObjectNew(testObject));
	}

	@Test
	public void testIsObjectManaged() {
		Object o = em.find(testObjectTwo.getClass(), testObjectTwo.getUri());
		assertTrue(testUOW.isObjectManaged(o));
		assertFalse(testUOW.isObjectManaged(null));
	}

	@Test
	public void testMergeChangesIntoParent() {
		OWLClassA testEntity = new OWLClassA();
		final URI pk = URI.create("http://testEntity");
		testEntity.setUri(pk);
		testEntity.setStringAttribute("testAtt");
		Object toRemove = em.find(testObject.getClass(), testObject.getUri());
		this.em.getTransaction().begin();
		em.persist(testEntity);
		this.em.remove(toRemove);
		this.em.getTransaction().commit();
		testEntity = em.find(testEntity.getClass(), testEntity.getUri());
		assertFalse(testUOW.getLiveObjectCache().contains(testObject.getClass(),
				IRI.create(testObject.getUri())));
	}

	@Test
	public void testRegisterAllObjects() {
		List<Object> testEntities = new ArrayList<Object>();
		testEntities.add(testObjectTwo);
		final OWLClassB tstOne = new OWLClassB();
		final URI pk = URI.create("http://tstOne");
		tstOne.setUri(pk);
		tstOne.setStringAttribute("tstOne");
		testEntities.add(tstOne);
		final OWLClassB tstTwo = new OWLClassB();
		final URI pkTwo = URI.create("http://tstTwo");
		tstTwo.setUri(pkTwo);
		tstTwo.setStringAttribute("tstTwo");
		testEntities.add(tstTwo);
		final int expectedSize = 3;
		Vector<Object> res = this.testUOW.registerAllObjects(testEntities);
		assertEquals(expectedSize, res.size());
	}

	@Test
	public void testRegisterExistingObject() {
		Object o = em.find(testObjectTwo.getClass(), testObjectTwo.getUri());
		OWLClassB orig = (OWLClassB) this.testUOW.getOriginal(o);
		OWLClassB clone = (OWLClassB) this.testUOW.registerExistingObject(orig);
		assertEquals(o, clone);
	}

	/**
	 * This method tests the situation when the Unit of Work has no clone to
	 * originals mapping - it was cleared. This tests the second branch of the
	 * register method.
	 */
	@Test
	public void testRegisterExistingObjectOnCleared() {
		this.em.clear();
		OWLClassA clone = this.em.find(OWLClassA.class, IRI.create(testObject.getUri()));
		assertEquals(testObject.getUri(), clone.getUri());
	}

	/**
	 * This method tests the branch where existing object is registered
	 */
	@Test
	public void testRegisterObject() {
		OWLClassA o = this.em.find(OWLClassA.class, IRI.create(testObject.getUri()));
		OWLClassA clone = (OWLClassA) this.testUOW.registerObject(o);
		assertNotNull(clone);
		assertEquals(o.getUri(), clone.getUri());
		assertEquals(o.getStringAttribute(), clone.getStringAttribute());
		assertEquals(o.getTypes(), clone.getTypes());
	}

	@Test
	public void testRegisterObjectWithNew() {
		final OWLClassA newOne = new OWLClassA();
		final URI pk = URI.create("http://newOne");
		newOne.setUri(pk);
		newOne.setStringAttribute("str");
		this.testUOW.registerObject(newOne);
		assertTrue(testUOW.contains(newOne));
		assertTrue(testUOW.getNewObjectsCloneToOriginal().containsKey(newOne));
	}

	@Test
	public void testRemoveObjectFromCache() {
		this.testUOW.removeObjectFromCache(testObjectTwo);
		assertFalse(this.testUOW.getLiveObjectCache().contains(testObjectTwo.getClass(),
				testObjectTwo.getUri()));
	}

	@Test
	public void testRegisterNewObject() {
		final OWLClassA newOne = new OWLClassA();
		final URI pk = URI.create("http://newEntity");
		newOne.setUri(pk);
		newOne.setStringAttribute("stringAttributeOne");
		this.testUOW.registerNewObject(newOne);
		assertTrue(testUOW.getNewObjectsCloneToOriginal().containsKey(newOne));
	}

	@Test(expected = NullPointerException.class)
	public void testRegisterNewObjectNull() {
		testUOW.registerNewObject(null);
		fail("This line should not have been reached.");
	}

	@Test(expected = NullPointerException.class)
	public void testRegisterNewObjectNullContext() {
		final OWLClassA newOne = new OWLClassA();
		final URI pk = URI.create("http://newEntity");
		newOne.setUri(pk);
		newOne.setStringAttribute("stringAttributeOne");
		testUOW.registerNewObject(newOne, null);
		fail("This line should not have been reached.");
	}

	@Test
	public void testPrimaryKeyAlreadyUsed() {
		final IRI testKey = IRI.create(testObject.getUri());
		assertTrue(testUOW.primaryKeyAlreadyUsed(testKey));
		final IRI testNotUsed = IRI.create("http://notUsed");
		assertFalse(testUOW.primaryKeyAlreadyUsed(testNotUsed));
	}

	@Test
	public void testReleaseUnitOfWork() {
		this.testUOW.release();
		assertTrue(testUOW.getCloneMapping().isEmpty());
		assertFalse(testUOW.isActive());
	}

	@Test
	public void testRemoveObject() {
		Object toRemove = em.find(testObject.getClass(), testObject.getUri());
		this.testUOW.removeObject(toRemove);
		assertTrue(testUOW.getDeletedObjects().containsKey(toRemove));
	}

	@Test
	public void testRemoveNewObject() {
		final OWLClassB newOne = new OWLClassB();
		final URI pk = URI.create("http://testObject");
		newOne.setUri(pk);
		newOne.setStringAttribute("strAtt");
		this.testUOW.registerNewObject(newOne);
		// Now try to remove it
		this.testUOW.removeObject(newOne);
		assertFalse(testUOW.contains(newOne));
	}

	@Test
	public void testUnregisterObject() {
		OWLClassA entity = em.find(OWLClassA.class, testObject.getUri());
		OWLClassA original = (OWLClassA) this.testUOW.getOriginal(entity);
		this.testUOW.unregisterObject(entity);
		assertFalse(testUOW.getLiveObjectCache().contains(original.getClass(), original.getUri()));
	}

	@Test
	public void revertObject() {
		OWLClassA clone = em.find(OWLClassA.class, testObject.getUri());
		assertNotNull(clone);
		final String changedAtt = "changedAtt";
		clone.setStringAttribute(changedAtt);
		this.testUOW.revertObject(clone);
		assertEquals(testObject.getStringAttribute(), clone.getStringAttribute());
	}

	@Test
	public void revertObjectReference() {
		OWLClassD clone = em.find(OWLClassD.class, testObjectThree.getUri());
		OWLClassA changedRef = new OWLClassA();
		final URI pk = URI.create("http://changedOne");
		changedRef.setStringAttribute("changedAtt");
		changedRef.setUri(pk);
		clone.setOwlClassA(changedRef);
		this.testUOW.revertObject(clone);
		assertEquals(testObject.getUri(), clone.getOwlClassA().getUri());
		assertEquals(testObject.getStringAttribute(), clone.getOwlClassA().getStringAttribute());
		assertNotNull(clone.getOwlClassA().getTypes());
	}
}
