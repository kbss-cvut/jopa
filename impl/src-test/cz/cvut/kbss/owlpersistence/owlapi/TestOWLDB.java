package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;
import java.util.logging.Logger;

import junit.framework.TestCase;
import cz.cvut.kbss.owlpersistence.model.EntityManager;

public class TestOWLDB extends TestCase {

	private Logger log = TestEnvironment.getLogger();

	public void testOWLDB() {
		EntityManager pc = TestEnvironment.getPersistenceConnector("TestOWLDB",
				true);
		pc.clear();
		OWLClassA t = new OWLClassA();
		t.setUri(URI.create("http://xyz/a"));
				
		pc.persist(t);

		pc.flush();

		// try {
		// pc.persist(t);
		// fail();
		// } catch (OWLPersistenceException e) {
		// log.info("Persisting failed - OK : " + e.getMessage());
		// return;
		// } finally {
		pc.close();
	}
}
