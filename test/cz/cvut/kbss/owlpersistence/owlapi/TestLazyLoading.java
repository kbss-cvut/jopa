package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;

import cz.cvut.kbss.owlpersistence.EntityManager;

public class TestLazyLoading {

	public static void main(String[] args) {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestLazyLoading-testSimple");

		OWLClassA a = new OWLClassA();
		URI uri = URI.create("http://newA");
		a.setUri(uri);

		a.setStringAttribute("new-value");

		pc.persist(a);

		pc.flush();

		pc.clear();

		final OWLClassA aX = pc.find(OWLClassA.class, uri);

		pc.close();
	}
}
