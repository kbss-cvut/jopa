package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;
import java.util.logging.Logger;

import junit.framework.TestCase;
import cz.cvut.kbss.owlpersistence.EntityManager;
import cz.cvut.kbss.owlpersistence.OWLPersistenceException;

public class TestPersistenceConnectorLogic extends TestCase {

	private Logger log = TestEnvironment.getLogger();

	public void testPersistWithoutPK() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testPersistWithoutPK");
		pc.clear();
		OWLClassA t = new OWLClassA();

		// try {
		pc.persist(t);
		// fail();
		// } catch (OWLPersistenceException e) {
		// log.info("Persisting failed - OK : " + e.getMessage());
		// return;
		// } finally {
		// pc.close();
		// }
	}

	public void testTwicePersist() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testTwicePersist");
		OWLClassA t = new OWLClassA();

		final URI pk = URI.create("http://newA");
		t.setUri(pk);

		pc.persist(t);

		try {
			pc.persist(t);
			fail();
		} catch (OWLPersistenceException e) {
			log.info("Persisting failed - OK : " + e.getMessage());
			return;
		} finally {
			pc.close();
		}
	}

	public void testTwicePersistDifferentClasses() {
		EntityManager pc = TestEnvironment
				.getPersistenceConnector("TestPersistenceConnectorLogic-testTwicePersistDifferentClasses");
		final URI pk = URI.create("http://newA");

		OWLClassA a = new OWLClassA();
		a.setUri(pk);
		OWLClassB b = new OWLClassB();
		b.setUri(pk);

		pc.persist(a);

		try {
			pc.persist(b);
			fail();
		} catch (OWLPersistenceException e) {
			log.info("Persisting failed - OK : " + e.getMessage());
			return;
		} finally {
			pc.close();
		}
	}
}
