package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.net.URI;
import java.util.Map;
import java.util.logging.Level;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import de.fraunhofer.iitb.owldb.OWLDBManager;
import de.fraunhofer.iitb.owldb.OWLDBOntology;
import de.fraunhofer.iitb.owldb.OWLDBOntologyFormat;
import de.fraunhofer.iitb.owldb.OWLDBOntologyOutputTarget;

/**
 * Implementation of OWLAPI storage connector for ontologies saved in RDBMS and
 * accessed through OWLDB.
 * 
 * @author kidney
 * 
 */
class OwlapiOwldbStorageConnector extends OwlapiStorageConnector {

	private IRI databaseIri;

	public OwlapiOwldbStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(storageProperties, properties);
	}

	@Override
	protected void initConnection(OntologyStorageProperties storageProperties)
			throws OntoDriverException {
		final URI logicalUri = storageProperties.getOntologyURI();
		final URI physicalUri = storageProperties.getPhysicalURI();
		final IRI ontologyIri = IRI.create(logicalUri);
		this.databaseIri = IRI.create(physicalUri);

		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Using database backend: " + physicalUri);
		}
		this.ontologyManager = OWLDBManager.createOWLOntologyManager(OWLDataFactoryImpl
				.getInstance());

		setIriMapper(logicalUri, physicalUri);

		this.dataFactory = this.ontologyManager.getOWLDataFactory();

		try {
			this.workingOntology = ontologyManager.loadOntology(ontologyIri);
		} catch (OWLOntologyCreationException e) {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Ontology " + logicalUri
						+ " does not exist in the database. Creating...");
			}
			createOntology(ontologyIri);
		}
		// Use this to pass properties to ontology
		// final Properties props = new Properties();
		// this.workingOnt = ((OWLDBOntologyManager)
		// ontologyManager).loadOntology(IRI.create(ontologyURI), props);
		saveWorkingOntology();
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Number of individuals in signature: "
					+ workingOntology.getIndividualsInSignature().size());
		}
	}

	private void createOntology(IRI ontologyUri) throws OntoDriverException {
		try {
			this.workingOntology = ontologyManager.createOntology(ontologyUri);
		} catch (OWLOntologyCreationException e) {
			throw new OntoDriverException(e);
		}
	}

	@Override
	public void saveWorkingOntology() throws OntoDriverException {
		// We must select the OWLDBOntologyFormat when storing in a
		// database
		final OWLDBOntologyFormat format = new OWLDBOntologyFormat();
		// Lets create a target with the provided target IRI
		final OWLDBOntologyOutputTarget target = new OWLDBOntologyOutputTarget(databaseIri);
		try {
			this.ontologyManager.saveOntology(workingOntology, format, target);
		} catch (OWLOntologyStorageException e) {
			throw new OntoDriverException(e);
		}
	}

	@Override
	public void close() throws OntoDriverException {
		super.close();
		((OWLDBOntology) workingOntology).destroyConnection();
	}
}
