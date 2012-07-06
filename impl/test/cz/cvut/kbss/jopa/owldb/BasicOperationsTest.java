package cz.cvut.kbss.jopa.owldb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;

import org.junit.After;
import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.owlapi.OWLClassA;
import cz.cvut.kbss.jopa.owlapi.OWLClassD;
import cz.cvut.kbss.jopa.owlapi.TestEnvironment;

public class BasicOperationsTest {

	private static EntityManager em;

	@After
	public void tearDown() throws Exception {
		if (em != null) {
			em.getEntityManagerFactory().close();
		}
	}

	@Test
	public void testFetchSimpleData() {
		em = TestEnvironment.getPersistenceConnector(
				"OWLDBTestBasicOperations-testFetchSimpleData", true, true);

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
				"OWLDBTestBasicOperations-testPersistEntity", true, true);
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
				"OWLDBTestBasicOperations-testPersistRelationship", true, true);
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
				"OWLDBTestBasicOperations-testRemoveEntity", true, true);
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
