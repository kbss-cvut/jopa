package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URI;
import java.net.URL;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import cz.cvut.kbss.owlpersistence.EntityManager;
import cz.cvut.kbss.owlpersistence.OWLPersistenceException;

import junit.framework.TestCase;

public class TestPersistenceConnectorLogic extends TestCase {

	private EntityManager pc = TestEnvironment.getPersistenceConnector();
	private Log log = TestEnvironment.getLog();
	
	public void testPersistWithoutPK() {
		pc.clear();
		OWLClassA t = new OWLClassA();

		try {
			pc.persist(t);
			fail();
		} catch (OWLPersistenceException e) {
			log.info("Persisting failed - OK : " + e.getMessage());
			return;
		}
	}

	public void testTwicePersist() {
		pc.clear();
		OWLClassA t = new OWLClassA();

		final URI pk = URI.create("http://new-OWLClassA");
		t.setUri(pk);

		pc.persist(t);

		try {
			pc.persist(t);
			fail();
		} catch (OWLPersistenceException e) {
			log.info("Persisting failed - OK : " + e.getMessage());
			return;
		}
	}
}
