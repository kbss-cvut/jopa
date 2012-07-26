package cz.cvut.kbss.jopa.accessors;

import java.net.URI;
import java.util.Map;
import java.util.logging.Level;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import de.fraunhofer.iitb.owldb.OWLDBManager;
import de.fraunhofer.iitb.owldb.OWLDBOntology;
import de.fraunhofer.iitb.owldb.OWLDBOntologyFormat;
import de.fraunhofer.iitb.owldb.OWLDBOntologyOutputTarget;

/**
 * Ontology accessor using OWLDB. TODO: Check the write/save strategy and fix
 * issues.
 * 
 * @author kidney
 * 
 */
public class OWLDBOntologyAccessor extends AccessStrategy {

	private IRI databaseIRI;

	protected OWLDBOntologyAccessor(Map<String, String> properties) {
		super(properties);
	}

	@Override
	protected void initConnection(Map<String, String> properties)
			throws OWLOntologyCreationException, OWLOntologyStorageException {
		final String ontologyURI = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY);
		final String dbConnection = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY);

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Using database backend: " + dbConnection);
		}
		if (dbConnection != null && !dbConnection.isEmpty()) {
			mapping.put(URI.create(ontologyURI), URI.create(dbConnection));
		}
		this.ontologyManager = OWLDBManager
				.createOWLOntologyManager(OWLDataFactoryImpl.getInstance());
		this.databaseIRI = IRI.create(dbConnection);

		parseMappings(ontologyURI);

		this.dataFactory = this.ontologyManager.getOWLDataFactory();
		final IRI ontoIri = IRI.create(ontologyURI);

		try {
			this.workingOntology = ontologyManager.loadOntology(ontoIri);
		} catch (OWLOntologyCreationException e) {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Ontology " + ontologyURI
						+ " does not exist in the database. Creating...");
			}
			createOntology(ontoIri);
		}
		// Use this to pass properties to ontology
		// final Properties props = new Properties();
		// this.workingOnt = ((OWLDBOntologyManager)
		// ontologyManager).loadOntology(IRI.create(ontologyURI), props);
		saveOntology();
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("INDS: "
					+ workingOntology.getIndividualsInSignature().size());
		}
	}

	protected void saveOntology() throws OWLOntologyStorageException {
		// We must select the OWLDBOntologyFormat when storing in a
		// database
		final OWLDBOntologyFormat format = new OWLDBOntologyFormat();
		// Lets create a target with the provided target IRI
		final OWLDBOntologyOutputTarget target = new OWLDBOntologyOutputTarget(
				databaseIRI);
		this.ontologyManager.saveOntology(workingOntology, format, target);
	}

	private void createOntology(IRI ontologyUri)
			throws OWLOntologyCreationException {
		this.workingOntology = ontologyManager.createOntology(ontologyUri);
	}

	@Override
	public void close() {
		super.close();
		// Not the nicest way, but OWLDB gives us no other chance
		((OWLDBOntology) workingOntology).destroyConnection();
	}

}
