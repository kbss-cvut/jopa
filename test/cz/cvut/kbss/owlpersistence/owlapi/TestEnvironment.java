package cz.cvut.kbss.owlpersistence.owlapi;

import java.io.File;
import java.net.URI;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.semanticweb.owl.apibinding.OWLManager;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyCreationException;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.model.OWLOntologyStorageException;
import org.semanticweb.owl.model.UnknownOWLOntologyException;

import cz.cvut.kbss.owlpersistence.EntityManager;

public class TestEnvironment {
	public static final Log log = LogFactory
			.getLog(TestPersistenceConnectorLogic.class);

	public static String dir = "testResults";

	public static EntityManager getPersistenceConnector(String name) {
		final OWLAPIPersistenceConnector pc = (OWLAPIPersistenceConnector) OWLPersistence.getEntityManager();

		final OWLOntologyManager m = OWLManager.createOWLOntologyManager();
		try {
			OWLOntology o = m
					.createOntology(URI
							.create("http://krizik.felk.cvut.cz/ontologies/2009/owlpersistence-tests/"
									+ name));
			final File url = new File(dir + "/" + name + ".owl");

			m.setPhysicalURIForOntology(o, url.toURI());
			m.saveOntology(o);

			pc.connect(url.toString());
			return pc;
		} catch (OWLOntologyCreationException e) {
			log.error(e, e);
			return null;
		} catch (UnknownOWLOntologyException e) {
			log.error(e, e);
			return null;
		} catch (OWLOntologyStorageException e) {
			log.error(e, e);
			return null;
		}
	}

	public static Log getLog() {
		return log;
	}
}
