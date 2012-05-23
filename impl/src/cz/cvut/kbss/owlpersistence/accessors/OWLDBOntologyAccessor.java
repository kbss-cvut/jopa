package cz.cvut.kbss.owlpersistence.accessors;

import java.util.Map;
import java.util.logging.Level;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import cz.cvut.kbss.owlpersistence.model.metamodel.Metamodel;
import cz.cvut.kbss.owlpersistence.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.owlpersistence.sessions.Session;
import de.fraunhofer.iitb.owldb.OWLDBManager;
import de.fraunhofer.iitb.owldb.OWLDBOntologyFormat;
import de.fraunhofer.iitb.owldb.OWLDBOntologyOutputTarget;

/**
 * Ontology accessor using OWLDB. TODO: Check the write/save strategy and fix
 * issues.
 * 
 * @author kidney
 * 
 */
public class OWLDBOntologyAccessor extends OWLOntologyAccessor {

	public OWLDBOntologyAccessor(Map<String, String> properties,
			Metamodel metamodel, Session session) {
		super(properties, metamodel, session);
	}

	@Override
	protected void initConnection(Map<String, String> properties)
			throws OWLOntologyCreationException, OWLOntologyStorageException {
		final String ontologyURI = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY);
		final String mappingFileURI = properties
				.get(OWLAPIPersistenceProperties.MAPPING_FILE_URI_KEY);
		final String dbConnection = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_DB_CONNECTION);

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Using database backend: " + dbConnection);
		}
		this.ontologyManager = OWLDBManager
				.createOWLOntologyManager(OWLDataFactoryImpl.getInstance());
		this.ontologyIRI = IRI.create(dbConnection);
		this.dataFactory = this.ontologyManager.getOWLDataFactory();

		parseMappings(mappingFileURI, ontologyURI);

		this.workingOnt = ontologyManager.loadOntology(IRI.create(ontologyURI));
		// We must select the OWLDBOntologyFormat when storing in a
		// database
		final OWLDBOntologyFormat format = new OWLDBOntologyFormat();
		// Lets create a target with the provided target IRI
		final OWLDBOntologyOutputTarget target = new OWLDBOntologyOutputTarget(
				ontologyIRI);
		this.ontologyManager.saveOntology(workingOnt, format, target);
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("INDS: " + workingOnt.getIndividualsInSignature().size());
		}
	}

	@Override
	protected void saveOntology() throws OWLOntologyStorageException {
		// We must select the OWLDBOntologyFormat when storing in a
		// database
		final OWLDBOntologyFormat format = new OWLDBOntologyFormat();
		// Lets create a target with the provided target IRI
		final OWLDBOntologyOutputTarget target = new OWLDBOntologyOutputTarget(
				ontologyIRI);
		this.ontologyManager.saveOntology(workingOnt, format, target);
	}

}
