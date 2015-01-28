package cz.cvut.kbss.jopa.test.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.test.OWLClassJ;
import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import cz.cvut.kbss.jopa.adapters.IndirectSet;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.TestEnvironment;

public class TestCollectionOperations {

	private static final Logger LOG = Logger.getLogger(TestCollectionOperations.class.getName());

	private static OWLClassA testA;
	private static Set<String> types;

	private EntityManager em;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		LOG.setLevel(Level.CONFIG);
		testA = new OWLClassA();
		final URI pk = URI.create("http://testA");
		testA.setUri(pk);
		testA.setStringAttribute("someStringAttribute");
		types = new HashSet<String>();
		for (int i = 0; i < 10; i++) {
			types.add("Type_" + i);
		}
		testA.setTypes(types);
	}

	@After
	public void tearDown() throws Exception {
		if (em.isOpen()) {
			em.close();
		}
	}

	@Test
	public void testPersistEntityWithCollection() {
		LOG.config("Test: persist entity with a simple collection of strings.");
		this.em = TestEnvironment.getPersistenceConnector(
				"TestCollectionOperations-persistEntityWithCollection", true);
		em.getTransaction().begin();
		em.persist(testA);
		assertTrue(testA.getTypes() instanceof IndirectSet);
		assertEquals(types, testA.getTypes());
		em.getTransaction().commit();
		assertFalse(testA.getTypes() instanceof IndirectSet);
		final OWLClassA res = em.find(OWLClassA.class, testA.getUri());
		assertNotNull(res);
		assertTrue(res.getTypes() instanceof IndirectSet);
		assertTrue(res.getTypes().containsAll(types));
	}

	@Test
	public void testDetachEntityWithCollection() {
		LOG.config("Test: persist entity and detach it before commit.");
		this.em = TestEnvironment.getPersistenceConnector(
				"TestCollectionOperations-detachEntityWithCollection", true);
		em.getTransaction().begin();
		em.persist(testA);
		assertTrue(em.contains(testA));
		em.getTransaction().commit();
		final OWLClassA res = em.find(OWLClassA.class, testA.getUri());
		assertNotNull(res);
		assertTrue(res.getTypes() instanceof IndirectSet);
		em.detach(res);
		assertFalse(res.getTypes() instanceof IndirectSet);
	}

	@Test
	public void testMergeDetachedEntityWithCollection() {
		LOG.config("Test: persist entity, detach it and merge again.");
		this.em = TestEnvironment.getPersistenceConnector(
				"TestCollectionOperations-mergeDetachedEntityWithCollection", true);
		em.getTransaction().begin();
		em.persist(testA);
		assertTrue(em.contains(testA));
		em.getTransaction().commit();
		OWLClassA res = em.find(OWLClassA.class, testA.getUri());
		assertNotNull(res);
		// Trigger lazy loading
		res.getTypes();
		em.detach(res);
		final String newString = "newStringAttribute";
		res.setStringAttribute(newString);
		res = em.merge(res);
		assertEquals(newString, res.getStringAttribute());
		assertTrue(res.getTypes() instanceof IndirectSet);
	}

	@Test
	public void testMergeDetachedEntityWithCollectionChanges() {
		LOG.config("Test: persist entity, detach it and merge again. Changes are made to the collection while detached.");
		this.em = TestEnvironment.getPersistenceConnector(
				"TestCollectionOperations-mergeDetachedEntityWithCollectionChanges", true);
		em.getTransaction().begin();
		em.persist(testA);
		assertTrue(em.contains(testA));
		em.getTransaction().commit();
		OWLClassA res = em.find(OWLClassA.class, testA.getUri());
		assertNotNull(res);
		em.detach(res);
		final String addedType = "addedTypeOne";
		res.getTypes().add(addedType);
		final String removed = res.getTypes().iterator().next();
		res.getTypes().remove(removed);
		res = em.merge(res);
		assertTrue(res.getTypes().contains(addedType));
		assertFalse(res.getTypes().contains(removed));
	}

	@Test
	public void testSetNewCollectionDuringTransaction() {
		LOG.config("Test: persist entity. Set new collection on it and make changes in the collection.");
		this.em = TestEnvironment.getPersistenceConnector(
				"TestCollectionOperations-setNewCollectionDuringTransaction", true);
		em.getTransaction().begin();
		em.persist(testA);
		em.getTransaction().commit();
		OWLClassA res = em.find(OWLClassA.class, testA.getUri());
		assertNotNull(res);
		final Set<String> newSet = new HashSet<String>();
		newSet.add("NewStringOne");
		newSet.add("NewStringTwo");
		em.getTransaction().begin();
		res.setTypes(newSet);
		res.getTypes().add("NewStringThree");
		em.getTransaction().commit();
		res = em.find(OWLClassA.class, testA.getUri());
		assertNotNull(res);
		final Set<String> resSet = res.getTypes();
		assertEquals(newSet.size(), resSet.size());
		assertTrue(newSet.containsAll(resSet));
	}

	@Test
	public void testAddIntoSimpleSet() {
		LOG.config("Test: update entity by adding an instance into simple set.");
		this.em = TestEnvironment.getPersistenceConnector(
				"TestCollectionOperations-addInstanceIntoSimpleSet", true);
		final OWLClassJ j = new OWLClassJ();
		j.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityJ"));
		final Set<OWLClassA> as = new HashSet<>();
		for (int i = 0; i < 5; i++) {
			final OWLClassA a = new OWLClassA();
			a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityA" + i));
			a.setStringAttribute("aaa");
			as.add(a);
		}
		j.setOwlClassA(as);
		final OWLClassA toAdd = new OWLClassA();
		toAdd.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#entityAAdded"));
		em.getTransaction().begin();
		em.persist(j);
		em.persist(toAdd);
		em.getTransaction().commit();

		em.getTransaction().begin();
		final OWLClassJ toUpdate = em.find(OWLClassJ.class, j.getUri());
		toUpdate.getOwlClassA();
		assertEquals(j.getOwlClassA().size(), toUpdate.getOwlClassA().size());
		toUpdate.getOwlClassA().add(toAdd);
		em.getTransaction().commit();

		final OWLClassJ res = em.find(OWLClassJ.class, j.getUri());
		assertEquals(j.getOwlClassA().size() + 1, res.getOwlClassA().size());
		boolean found = false;
		for (OWLClassA a : res.getOwlClassA()) {
			if (a.getUri().equals(toAdd.getUri())) {
				found = true;
				break;
			}
		}
		assertTrue(found);
	}
}
