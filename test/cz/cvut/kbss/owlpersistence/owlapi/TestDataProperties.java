package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;

import org.apache.commons.logging.Log;

import cz.cvut.kbss.owlpersistence.EntityManager;

import junit.framework.TestCase;

public class TestDataProperties extends TestCase {
	private EntityManager pc = TestEnvironment.getPersistenceConnector();
	private Log log = TestEnvironment.getLog();

	public void testPersistFound() {
		OWLClassA t = new OWLClassA();

		final URI pk = URI.create("http://new-class");

		t.setUri(pk);

		pc.persist(t);

		final OWLClassA a = pc.find(OWLClassA.class, pk);

		log.info("Persisted " + t + ", found " + a);
		assertEquals(a, t);
	}

	public void testRemove() {
		OWLClassA t = new OWLClassA();

		final URI pk = URI.create("http://new-class");

		t.setUri(pk);

		pc.persist(t);

		log.info("Persisted " + t);
		assertTrue(pc.contains(t));

		pc.remove(t);

		log.info("Removed " + t);
		assertFalse(pc.contains(t));
	}

}
