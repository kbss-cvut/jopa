package cz.cvut.kbss.owlpersistence.owlapi;

import java.io.File;
import java.net.MalformedURLException;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.model.UnknownOWLOntologyException;

import cz.cvut.kbss.owlpersistence.EntityManager;

public class TestEnvironment {
	public static final Logger log = Logger
			.getLogger(TestPersistenceConnectorLogic.class.getName());

	public static String dir = "testResults";

	public static EntityManager getPersistenceConnector(String name) {
		final OWLAPIPersistenceConnector pc = (OWLAPIPersistenceConnector) OWLPersistence
				.getEntityManager();

		final OWLOntologyManager m = OWLManager.createOWLOntologyManager();
		try {
			OWLOntology o = m
					.createOntology(IRI
							.create("http://krizik.felk.cvut.cz/ontologies/2009/owlpersistence-tests/"
									+ name));
			final File url = new File(dir + "/" + name + ".owl");

			m.setPhysicalURIForOntology(o, url.toURI());
			m.saveOntology(o);

			pc.connect(Collections.singletonMap("ontologyURI", url.toURI()
					.toURL().toString()));
			return pc;
		} catch (OWLOntologyCreationException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
			return null;
		} catch (UnknownOWLOntologyException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
			return null;
		} catch (OWLOntologyStorageException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
			return null;
		} catch (MalformedURLException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
			return null;
		}
	}

	public static Logger getLogger() {
		return log;
	}
}
