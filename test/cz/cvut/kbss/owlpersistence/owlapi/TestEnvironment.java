package cz.cvut.kbss.owlpersistence.owlapi;

import java.net.URL;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import cz.cvut.kbss.owlpersistence.EntityManager;

public class TestEnvironment {
	public static final Log log = LogFactory
			.getLog(TestPersistenceConnectorLogic.class);

	private static final URL u = TestPersistenceConnectorLogic.class
			.getResource("/test-ontology.owl");
	private static final OWLAPIPersistenceConnector pc = new OWLAPIPersistenceConnector();

	{
		pc.connect(u.toString());
	}

	public static EntityManager getPersistenceConnector() {
		return pc;
	}
	
	public static Log getLog() {
		return log;
	}
}
