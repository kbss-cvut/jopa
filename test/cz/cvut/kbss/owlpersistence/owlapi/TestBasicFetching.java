package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;
import cz.cvut.kbss.owlpersistence.model.EntityManager;

public class TestBasicFetching extends TestCase {

	public void testFetchSimpleData() {
		EntityManager em = TestEnvironment
				.getPersistenceConnector("TestBasicFetching-testFetchSimpleData");

		OWLClassA a = new OWLClassA();
		URI uri = URI.create("http://newA");
		a.setUri(uri);

		a.setStringAttribute("new-value");

		em.persist(a);

		assertTrue(em.contains(a));

		em.flush();

		em.clear();

		assertFalse(em.contains(a));

		final OWLClassA aX = em.find(OWLClassA.class, uri);

		assertNotNull(aX);

		assertEquals(aX.getStringAttribute(), "new-value");

		em.close();
	}

	public void testFetchReferences() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestBasicFetching-testFetchSimpleReference");

		OWLClassA a = new OWLClassA();
		URI uriA = URI.create("http://newA");
		a.setUri(uriA);

		a.setStringAttribute("new-value");

		OWLClassD d = new OWLClassD();
		URI uriD = URI.create("http://newD");
		d.setUri(uriD);

		d.setOwlClassA(a);

		// more references
		OWLClassA a2 = new OWLClassA();
		URI uriA2 = URI.create("http://newA2");
		a2.setUri(uriA2);
		OWLClassC c = new OWLClassC();
		URI uriC = URI.create("http://newC");
		c.setUri(uriC);

		c.setReferencedList(Arrays.asList(a, a2));
		c.setSimpleList(Arrays.asList(a, a2));

		pc.persist(a);
		pc.persist(a2);
		pc.persist(c);
		pc.persist(d);

		assertTrue(pc.contains(a));
		assertTrue(pc.contains(d));

		pc.flush();

		pc.clear();

		assertFalse(pc.contains(a));
		assertFalse(pc.contains(a2));
		assertFalse(pc.contains(c));
		assertFalse(pc.contains(d));

		final OWLClassD dX = pc.find(OWLClassD.class, uriD);

		assertNotNull(dX);

		assertEquals(dX.getOwlClassA().getStringAttribute(), "new-value");

		final OWLClassC cX = pc.find(OWLClassC.class, uriC);

		assertEquals(2, cX.getReferencedList().size());

		assertEquals(2, cX.getSimpleList().size());

		pc.close();
	}

	public void testFetchHugeReferences() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestBasicFetching-testFetchHugeReferences");

		OWLClassC c = new OWLClassC();
		URI uriC = URI.create("http://newC");
		c.setUri(uriC);

		List<OWLClassA> list = new ArrayList<OWLClassA>();
		for (int i = 0; i < 100; i++) {
			OWLClassA a = new OWLClassA();
			URI uriA = URI.create("http://newA-" + i);
			a.setUri(uriA);
			a.setStringAttribute("new-value");
			list.add(a);
			pc.persist(a);
		}

		c.setReferencedList(list);

		pc.persist(c);

		pc.flush();

		pc.clear();

		final OWLClassC cX = pc.find(OWLClassC.class, uriC);

		// assertEquals(100, cX.getReferencedList().size());

		pc.close();
	}

	public static void main(String[] args) {
		new TestBasicFetching().testFetchSimpleData();
	}
}
