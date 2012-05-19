package cz.cvut.kbss.owlpersistence.owldb;

import static org.junit.Assert.*;

import java.net.URI;

import org.junit.Test;

import cz.cvut.kbss.owlpersistence.model.EntityManager;
import cz.cvut.kbss.owlpersistence.owlapi.OWLClassA;
import cz.cvut.kbss.owlpersistence.owlapi.TestEnvironment;

public class BasicOperationsTest {

	@Test
	public void testFetchSimpleData() {
		EntityManager em = TestEnvironment.getPersistenceConnector(
				"TestBasicFetching-testFetchSimpleData", true, true);

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

		em.close();
	}

}
