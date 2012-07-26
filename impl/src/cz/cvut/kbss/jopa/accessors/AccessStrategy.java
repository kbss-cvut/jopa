package cz.cvut.kbss.jopa.accessors;

import java.io.File;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.util.OWLOntologyMerger;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.util.MappingFileParser;

abstract class AccessStrategy {

	protected static final Logger LOG = Logger.getLogger(AccessStrategy.class
			.getName());
	private static final String FILE_PREFIX = "file:";
	private static final String OWLDB_PREFIX = "jdbc";

	protected static Map<URI, URI> mapping;

	protected OWLReasoner reasoner;
	protected OWLOntology workingOntology;
	protected OWLOntology reasoningOntology;
	protected OWLOntologyManager ontologyManager;
	protected OWLReasonerFactory reasonerFactory;
	protected OWLDataFactory dataFactory;

	protected AccessStrategy() {
		super();
	}

	protected AccessStrategy(final Map<String, String> properties) {
		super();
		init(properties);
	}

	static AccessStrategy getStrategy(final Map<String, String> properties) {
		final String ontologyURI = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY);
		final String mappingFileURI = properties
				.get(OWLAPIPersistenceProperties.MAPPING_FILE_URI_KEY);
		final String ontologyPhysicalURI = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY);

		mapping = getMappings(mappingFileURI);
		switch (getStorageType(ontologyURI, ontologyPhysicalURI)) {
		case FILE:
			return new OWLFileOntologyAccessor(properties);
		case OWLDB:
			return new OWLDBOntologyAccessor(properties);
		}
		// More strategies can be put here
		return null;
	}

	private static OntologyStorageType getStorageType(String ontologyURI,
			String ontologyPhysicalURI) {
		if (ontologyPhysicalURI != null) {
			if (ontologyPhysicalURI.startsWith(OWLDB_PREFIX)) {
				return OntologyStorageType.OWLDB;
			} else {
				return OntologyStorageType.FILE;
			}
		}
		final URI physUri = mapping.get(URI.create(ontologyURI));
		if (physUri == null) {
			throw new OWLPersistenceException(
					"Unable to determine storage type and location. Specify either mapping file or physical location.");
		}
		if (physUri.getScheme().startsWith(OWLDB_PREFIX)) {
			return OntologyStorageType.OWLDB;
		} else {
			return OntologyStorageType.FILE;
		}
	}

	private void init(final Map<String, String> properties) {
		final String ontologyURI = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY);
		final String mappingFileURI = properties
				.get(OWLAPIPersistenceProperties.MAPPING_FILE_URI_KEY);
		final String reasonerFactoryClass = properties
				.get(OWLAPIPersistenceProperties.REASONER_FACTORY_CLASS);
		try {
			this.reasonerFactory = (OWLReasonerFactory) Class.forName(
					reasonerFactoryClass).newInstance();
		} catch (Exception e) {
			throw new OWLPersistenceException("Error instantiating factory '"
					+ reasonerFactoryClass + "'.", e);
		}

		if (ontologyURI == null) {
			throw new IllegalArgumentException(
					"Either a document URL or an ontology URI must be specified.");
		}

		if (LOG.isLoggable(Level.INFO)) {
			LOG.info("Loading model ontologyURI='" + ontologyURI
					+ "', mappingFileURI='" + mappingFileURI + "'.");
		}
		try {

			initConnection(properties);

			this.reasoningOntology = new OWLOntologyMerger(this.ontologyManager)
					.createMergedOntology(this.ontologyManager,
							IRI.create("http://temporary"));
			LOG.info("Ontology " + ontologyURI + " succesfully loaded.");
		} catch (Exception e) {
			LOG.log(Level.SEVERE, null, e);
			throw new OWLPersistenceException(e);
		}
		try {
			this.reasoner = this.reasonerFactory
					.createReasoner(reasoningOntology);
			this.reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	abstract protected void initConnection(Map<String, String> properties)
			throws OWLOntologyCreationException, OWLOntologyStorageException;

	abstract protected void saveOntology() throws OWLOntologyStorageException;

	OWLReasoner getReasoner() {
		return reasoner;
	}

	OWLOntology getWorkingOntology() {
		return workingOntology;
	}

	OWLOntology getReasoningOntology() {
		return reasoningOntology;
	}

	OWLOntologyManager getOntologyManager() {
		return ontologyManager;
	}

	OWLReasonerFactory getReasonerFactory() {
		return reasonerFactory;
	}

	OWLDataFactory getDataFactory() {
		return dataFactory;
	}

	/**
	 * Load the mappings from mapping file.
	 * 
	 * @param mappingFileURI
	 * @return Map<URI, URI>
	 */
	private static Map<URI, URI> getMappings(String mappingFileURI) {
		final Map<URI, URI> mapping;
		if (mappingFileURI != null) {
			mapping = MappingFileParser.getMappings(new File(URI
					.create(mappingFileURI)));
		} else {
			mapping = new HashMap<URI, URI>();
		}

		return mapping;
	}

	/**
	 * Parse mappings and return physical URI of the ontology.
	 * 
	 * @param mappingFileURI
	 * @param ontologyURI
	 * @param additionalMapping
	 *            Additional mapping, e. g. from properties passed to the
	 *            accessor. This is optional, if you don't want to use it, pass
	 *            an empty map.
	 * @return
	 */
	protected URI parseMappings(String ontologyURI) {
		LOG.info("Found mappings = " + mapping);

		ontologyManager.addIRIMapper(new OWLOntologyIRIMapper() {
			public IRI getDocumentIRI(IRI arg0) {
				if (!mapping.containsKey(arg0.toURI())) {
					return arg0;
				}

				return IRI.create(mapping.get(arg0.toURI()));
			}
		});
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("Mapping file succesfully parsed.");
		}
		return mapping.get(URI.create(ontologyURI));
	}

	public void close() {
		// do nothing here
	}

}
