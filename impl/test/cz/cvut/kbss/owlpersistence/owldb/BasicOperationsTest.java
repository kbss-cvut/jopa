package cz.cvut.kbss.owlpersistence.owldb;

import static org.junit.Assert.*;

import java.net.URI;

import org.junit.Test;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassA;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassD;
import cz.cvut.kbss.owlpersistence.owlapi.TestEnvironment;

public class BasicOperationsTest {

	@Test
	public void testFetchSimpleData() {
		EntityManager em = TestEnvironment.getPersistenceConnector("owldb",
				true, true);

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

		em.getEntityManagerFactory().close();
	}

	@Test
	public void testPersistEntity() {
		EntityManager em = TestEnvironment.getPersistenceConnector("owldb",
				true, true);
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

		em.getEntityManagerFactory().close();
	}

	@Test
	public void testPersistRelationship() {
		EntityManager em = TestEnvironment.getPersistenceConnector("owldb",
				true, true);
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

		em.getEntityManagerFactory().close();
	}

	@Test
	public void testRemoveEntity() {
		EntityManager em = TestEnvironment.getPersistenceConnector("owldb",
				true, true);
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

		em.clear();
		em.getEntityManagerFactory().close();
	}

}
