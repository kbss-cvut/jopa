package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.io.File;
import java.util.Map;
import java.util.logging.Level;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.OWLOntologyCreationIOException;
import org.semanticweb.owlapi.io.OWLOntologyInputSourceException;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

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

		if (physicalUri != null) {
			OwlapiUtils.setOntologyManagerIriMapper(ontologyManager, ontologyUri, physicalUri);
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Loading ontology from file: " + physicalUri.toString());
			}
			try {
				this.workingOntology = ontologyManager.loadOntology(IRI.create(ontologyUri));
			} catch (OWLOntologyInputSourceException e) {
				createOntology();
			} catch (OWLOntologyCreationIOException e) {
				createOntology();
			} catch (OWLOntologyCreationException e) {
				throw new OntoDriverException(e);
			}
		} else {
			this.physicalUri = ontologyUri;
			OwlapiUtils.setOntologyManagerIriMapper(ontologyManager, ontologyUri, ontologyUri);
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Loading ontology from logical URI: " + ontologyUri);
			}
			try {
				this.workingOntology = ontologyManager.loadOntology(IRI.create(ontologyUri));
			} catch (OWLOntologyCreationException e) {
				throw new OntoDriverException(e);
			}
		}
	}

	@Override
	protected void reloadOntology() throws OntoDriverException {
		try {
			this.workingOntology = ontologyManager.loadOntology(IRI.create(ontologyUri));
		} catch (OWLOntologyInputSourceException e) {
			throw new OntoDriverException(e);
		} catch (OWLOntologyCreationException e) {
			throw new OntoDriverException(e);
		}
	}

	/**
	 * Creates ontology in the physical location. </p>
	 * 
	 * This method is called when loading ontology from {@code physicalUri}
	 * leads to an exception caused by non existing ontology at the target
	 * location.
	 * 
	 * @throws OntoDriverException
	 *             If the ontology cannot be created
	 */
	private void createOntology() throws OntoDriverException {
		try {
			final OWLOntology newOnto = ontologyManager.createOntology(IRI.create(ontologyUri));
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
