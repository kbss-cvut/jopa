package cz.cvut.kbss.owlpersistence.owlapi;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.model.UnknownOWLOntologyException;

import cz.cvut.kbss.owlpersistence.Persistence;
import cz.cvut.kbss.owlpersistence.model.EntityManager;

public class TestEnvironment {
	public static final Logger log = Logger
			.getLogger(TestPersistenceConnectorLogic.class.getName());

	public static String dir = "testResults";

	private static final String REASONER_FACTORY_CLASS = "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory";
//	private static final String REASONER_FACTORY_CLASS = "org.semanticweb.HermiT.Reasoner$ReasonerFactory";
	
	public static EntityManager getPersistenceConnector(String name) {
		try {
			final OWLOntologyManager m = OWLManager.createOWLOntologyManager();
			final IRI iri = IRI
					.create("http://krizik.felk.cvut.cz/ontologies/2009/owlpersistence-tests/"
							+ name);
			OWLOntology o = m.createOntology(iri);
			final File url = new File(dir + "/" + name + ".owl");

			m.saveOntology(o, IRI.create(url.toURI()));

			final Map<String, String> params = new HashMap<String, String>();

			params.put("javax.persistence.provider",
					EntityManagerFactoryImpl.class.getName());

			// params.put(OWLAPIPersistenceProperties.ONTOLOGY_DB_CONNECTION,
			// "jdbc:postgresql://localhost/strufail_owlapi");
			params.put(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY, url
					.toURI().toString());
			// params.put(OWLAPIPersistenceProperties.ONTOLOGY_FILE_KEY, url
			// .getAbsolutePath());
			params.put(OWLAPIPersistenceProperties.JPA_PERSISTENCE_PROVIDER,
					OWLAPIPersistenceProvider.class.getName());
			params.put(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS,
					REASONER_FACTORY_CLASS);

			return Persistence.createEntityManagerFactory("context-name",
					params).createEntityManager();
		} catch (OWLOntologyCreationException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
		} catch (UnknownOWLOntologyException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
		} catch (OWLOntologyStorageException e) {
			log.log(Level.SEVERE, e.getMessage(), e);
		}
		return null;
	}

	public static Logger getLogger() {
		return log;
	}
}
