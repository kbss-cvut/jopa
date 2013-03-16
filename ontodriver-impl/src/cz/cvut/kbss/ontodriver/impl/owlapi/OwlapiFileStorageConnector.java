package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.io.File;
import java.net.URI;
import java.util.Map;
import java.util.logging.Level;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.OWLOntologyInputSourceException;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.utils.OwlapiUtils;

/**
 * Implementation of OWLAPI storage connector for ontologies saved in a regular
 * file.
 * 
 * @author kidney
 * 
 */
class OwlapiFileStorageConnector extends OwlapiStorageConnector {

	public OwlapiFileStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(storageProperties, properties);
	}

	@Override
	protected void initConnection(OntologyStorageProperties storageProperties)
			throws OntoDriverException {
		this.ontologyManager = OWLManager.createOWLOntologyManager();
		this.dataFactory = this.ontologyManager.getOWLDataFactory();

		final URI logicalUri = storageProperties.getOntologyURI();
		final URI physicalUri = storageProperties.getPhysicalURI();

		if (physicalUri != null) {
			OwlapiUtils.setOntologyManagerIriMapper(ontologyManager, logicalUri, physicalUri);
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Loading ontology from file: " + physicalUri.toString());
			}
			try {
				this.workingOntology = ontologyManager.loadOntologyFromOntologyDocument(new File(
						physicalUri));
			} catch (OWLOntologyInputSourceException e) {
				createOntology(logicalUri, physicalUri);
			} catch (OWLOntologyCreationException e) {
				throw new OntoDriverException(e);
			}
		} else {
			OwlapiUtils.setOntologyManagerIriMapper(ontologyManager, logicalUri, logicalUri);
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Loading ontology from logical URI: " + logicalUri);
			}
			try {
				this.workingOntology = ontologyManager.loadOntology(IRI.create(logicalUri));
			} catch (OWLOntologyCreationException e) {
				throw new OntoDriverException(e);
			}
		}
	}

	/**
	 * Creates ontology in the physical location. </p>
	 * 
	 * This method is called when loading ontology from {@code physicalUri}
	 * leads to an exception caused by non existing ontology at the target
	 * location.
	 * 
	 * @param logicalUri
	 *            Logical URI of the ontology
	 * @param physicalUri
	 *            Physical URI of the ontology storage
	 * @throws OntoDriverException
	 *             If the ontology cannot be created
	 */
	private void createOntology(URI logicalUri, URI physicalUri) throws OntoDriverException {
		try {
			final OWLOntology newOnto = ontologyManager.createOntology(IRI.create(logicalUri));
			ontologyManager.saveOntology(newOnto, IRI.create(physicalUri));
			ontologyManager.removeOntology(newOnto);
			this.workingOntology = this.ontologyManager.loadOntologyFromOntologyDocument(new File(
					physicalUri));
		} catch (OWLOntologyCreationException e) {
			throw new OntoDriverException("Unable to create ontology.", e);
		} catch (OWLOntologyStorageException e) {
			throw new OntoDriverException("Unable to create ontology.", e);
		}
	}

}
