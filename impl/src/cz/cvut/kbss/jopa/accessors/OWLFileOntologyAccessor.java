package cz.cvut.kbss.jopa.accessors;

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

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;

/**
 * Ontology accessor using regular files.
 * 
 * @author kidney
 * 
 */
public class OWLFileOntologyAccessor extends AccessStrategy {

	protected OWLFileOntologyAccessor(final Map<String, String> properties) {
		super(properties);
	}

	@Override
	protected void initConnection(Map<String, String> properties)
			throws OWLOntologyCreationException, OWLOntologyStorageException {
		// The ontologyURI will be eventually read from an XML configuration
		// file
		// And the properties will be able to override this configuration
		final String ontologyURI = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY);
		final String ontologyPhysicalURI = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY);

		this.ontologyManager = OWLManager.createOWLOntologyManager();
		this.dataFactory = this.ontologyManager.getOWLDataFactory();

		URI physicalURI = parseMappings(ontologyURI);

		if (physicalURI == null) {
			if (ontologyPhysicalURI != null && !ontologyPhysicalURI.isEmpty()) {
				final File ontoFile = new File(ontologyPhysicalURI);
				if (!ontoFile.isAbsolute()) {
					physicalURI = URI.create("file://"
							+ ontoFile.getAbsolutePath());
				} else {
					physicalURI = URI.create("file://" + ontologyPhysicalURI);
				}
			} else if (ontologyURI.startsWith("file:")) {
				physicalURI = URI.create(ontologyURI);
			}
		}

		if (physicalURI != null) {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Loading ontology from file: "
						+ physicalURI.toString());
			}
			try {
				this.workingOntology = ontologyManager
						.loadOntologyFromOntologyDocument(new File(physicalURI));
			} catch (OWLOntologyInputSourceException e) {
				createOntology(ontologyURI, physicalURI);
			}
		} else {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Loading ontology from logical URI: " + ontologyURI);
			}
			this.workingOntology = ontologyManager.loadOntology(IRI
					.create(ontologyURI));
		}

	}

	private void createOntology(final String ontologyURI, final URI physicalURI) {
		try {
			final OWLOntology newOnto = ontologyManager.createOntology(IRI
					.create(ontologyURI));
			ontologyManager.saveOntology(newOnto, IRI.create(physicalURI));
			ontologyManager.removeOntology(newOnto);
			this.workingOntology = this.ontologyManager
					.loadOntologyFromOntologyDocument(new File(physicalURI));
		} catch (OWLOntologyCreationException e) {
			throw new OWLPersistenceException("Unable to create ontology.", e);
		} catch (OWLOntologyStorageException e) {
			throw new OWLPersistenceException("Unable to create ontology.", e);
		}
	}

	protected void saveOntology() throws OWLOntologyStorageException {
		ontologyManager.saveOntology(workingOntology);
	}

}
