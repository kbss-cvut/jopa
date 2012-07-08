package cz.cvut.kbss.jopa.accessors;

import java.io.File;
import java.net.URI;
import java.util.Collections;
import java.util.Map;
import java.util.logging.Level;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.OWLOntologyInputSourceException;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.sessions.Session;

/**
 * Ontology accessor using regular files.
 * 
 * @author kidney
 * 
 */
public class OWLFileOntologyAccessor extends OWLOntologyAccessor {

	public OWLFileOntologyAccessor(Map<String, String> properties,
			Metamodel metamodel, Session session) {
		super(properties, metamodel, session);
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
		final String mappingFileURI = properties
				.get(OWLAPIPersistenceProperties.MAPPING_FILE_URI_KEY);

		this.ontologyManager = OWLManager.createOWLOntologyManager();
		this.dataFactory = this.ontologyManager.getOWLDataFactory();

		URI physicalURI = parseMappings(mappingFileURI, ontologyURI,
				Collections.<URI, URI> emptyMap());

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
				this.workingOnt = this.ontologyManager
						.loadOntologyFromOntologyDocument(new File(physicalURI));
			} catch (OWLOntologyInputSourceException e) {
				createOntology(ontologyURI, physicalURI);
			}
		} else {
			if (LOG.isLoggable(Level.CONFIG)) {
				LOG.config("Loading ontology from logical URI: " + ontologyURI);
			}
			this.workingOnt = this.ontologyManager.loadOntology(IRI
					.create(ontologyURI));
		}

	}

	private void createOntology(final String ontologyURI, final URI physicalURI) {
		try {
			final OWLOntology newOnto = ontologyManager.createOntology(IRI
					.create(ontologyURI));
			ontologyManager.saveOntology(newOnto, IRI.create(physicalURI));
			ontologyManager.removeOntology(newOnto);
			this.workingOnt = this.ontologyManager
					.loadOntologyFromOntologyDocument(new File(physicalURI));
		} catch (OWLOntologyCreationException e) {
			throw new OWLPersistenceException("Unable to create ontology.", e);
		} catch (OWLOntologyStorageException e) {
			throw new OWLPersistenceException("Unable to create ontology.", e);
		}
	}

	@Override
	protected void saveOntology() throws OWLOntologyStorageException {
		this.ontologyManager.saveOntology(workingOnt);
	}

}
