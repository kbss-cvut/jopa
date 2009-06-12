package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.apache.commons.logging.Log;

import cz.cvut.kbss.owlpersistence.EntityManager;

import junit.framework.TestCase;

public class TestBasicInteractions extends TestCase {
	private Log log = TestEnvironment.getLog();

	public void testPersistFound() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestBasicInteractions-testPersistFound");
		OWLClassA t = new OWLClassA();

		final URI pk = URI.create("http://new-class");

		t.setUri(pk);

		pc.persist(t);

		final OWLClassA a = pc.find(OWLClassA.class, pk);

		log.info("Persisted " + t + ", found " + a);
		assertEquals(a, t);
		pc.close();
	}

	public void testRemove() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestBasicInteractions-testRemove");
		OWLClassA t = new OWLClassA();

		final URI pk = URI.create("http://new-class");

		t.setUri(pk);

		pc.persist(t);

		log.info("Persisted " + t);
		assertTrue(pc.contains(t));

		pc.remove(t);

		log.info("Removed " + t);
		assertFalse(pc.contains(t));
		pc.close();
	}

	public void testSingleDataReference() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestBasicInteractions-testSingleDataReference");
		OWLClassA a = new OWLClassA();
		final URI pkA = URI.create("http://newA");
		a.setUri(pkA);

		pc.persist(a);

		a.setStringAttribute("testValue");

		pc.close();

	}

	public void testSingleOWLClassReference() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestBasicInteractions-testSingleOWLClassReference");
		OWLClassA a = new OWLClassA();
		final URI pkA = URI.create("http://newA");
		a.setUri(pkA);

		pc.persist(a);

		OWLClassD d = new OWLClassD();
		final URI pkD = URI.create("http://newD");
		d.setUri(pkD);

		d.setOwlClassA(a);

		pc.persist(d);

		pc.close();

	}

	public void testArrayOWLClassReference() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestBasicInteractions-testArrayOWLClassReference");
		OWLClassA a = new OWLClassA();
		final URI pkA = URI.create("http://newA-1");
		a.setUri(pkA);

		OWLClassC c = new OWLClassC();
		final URI pkC = URI.create("http://newC-1");
		c.setUri(pkC);

		c.setList(Collections.singletonList(a));

		pc.persist(c);
		pc.persist(a);

		pc.close();

	}

	public void testArrayOWLClassReference2() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestBasicInteractions-testArrayOWLClassReference");

		List<OWLClassA> list = new ArrayList<OWLClassA>();

		// 486
		for (int i = 0; i < 40; i++) {
			OWLClassA a = new OWLClassA();
			final URI pkA = URI.create("http://newA1-" + i);
			a.setUri(pkA);
			list.add(a);
			pc.persist(a);
		}

		OWLClassC c = new OWLClassC();
		final URI pkC = URI.create("http://newC-2");
		c.setUri(pkC);
		c.setList(list);

		pc.persist(c);

		pc.close();
	}
}
