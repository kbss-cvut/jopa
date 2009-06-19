package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;
import java.util.Collections;

import cz.cvut.kbss.owlpersistence.EntityManager;

public class TestLazyLoading {

	public static void main(String[] args) {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestLazyLoading-testSimple");

		OWLClassA a = new OWLClassA();
		URI uri = URI.create("http://newA");
		a.setUri(uri);

		a.setStringAttribute("new-value");

		OWLClassC c = new OWLClassC();
		URI uriC = URI.create("http://newC");
		c.setUri(uriC);

		c.setReferencedList(Collections.singletonList(a));
		
		pc.persist(a);
		pc.persist(c);

		pc.flush();

		pc.clear();

		final OWLClassC aX = pc.find(OWLClassC.class, uriC);

		System.out.println(aX.getReferencedList());
		
		pc.close();
	}
}
