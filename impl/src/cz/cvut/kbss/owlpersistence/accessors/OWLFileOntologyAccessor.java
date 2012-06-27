package cz.cvut.kbss.owlpersistence.accessors;

import java.io.File;
import java.net.URI;
import java.util.Map;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import cz.cvut.kbss.owlpersistence.model.metamodel.Metamodel;
import cz.cvut.kbss.owlpersistence.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.owlpersistence.sessions.Session;

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
			throws OWLOntologyCreationException {
		final String ontologyURI = properties
				.get(OWLAPIPersistenceProperties.ONTOLOGY_URI_KEY);
		final String mappingFileURI = properties
				.get(OWLAPIPersistenceProperties.MAPPING_FILE_URI_KEY);

		this.ontologyManager = OWLManager.createOWLOntologyManager();
		this.dataFactory = this.ontologyManager.getOWLDataFactory();

		URI physicalURI = parseMappings(mappingFileURI, ontologyURI);

		if (physicalURI == null) {
			physicalURI = URI.create(ontologyURI);
		}

		if (physicalURI != null) {
			this.workingOnt = this.ontologyManager
					.loadOntologyFromOntologyDocument(new File(physicalURI));
		} else if (ontologyURI.startsWith("file:")) {
			this.workingOnt = this.ontologyManager
					.loadOntologyFromOntologyDocument(new File(URI
							.create(ontologyURI)));
		} else {
			this.workingOnt = this.ontologyManager.loadOntology(IRI
					.create(ontologyURI));
		}
	}

	@Override
	protected void saveOntology() throws OWLOntologyStorageException {
		this.ontologyManager.saveOntology(workingOnt);
	}

}
