package cz.cvut.kbss.owlpersistence.owlapi;

import java.io.File;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import cz.cvut.kbss.owlpersistence.util.MappingFileParser;
import de.fraunhofer.iitb.owldb.OWLDBManager;
import de.fraunhofer.iitb.owldb.OWLDBOntologyFormat;
import de.fraunhofer.iitb.owldb.OWLDBOntologyManager;
import de.fraunhofer.iitb.owldb.OWLDBOntologyOutputTarget;

public class FileToOWLDBConverter {

	public static void main(String[] args) {

		final String ontologyURI = "http://krizik.felk.cvut.cz/ontologies/2009/utam-failures.owl";
		final String dbConnection = "jdbc:postgresql://localhost/strufail_owlapi";
		final String mappingFileURI = "file:///home/kremen/fel/projects/utam/internal-svn/utam-failures/impl/runtime/mapping";

		OWLDBOntologyManager m = (OWLDBOntologyManager) OWLDBManager
				.createOWLOntologyManager(OWLDataFactoryImpl.getInstance());

		final Map<URI, URI> mapping = getMappings(mappingFileURI);

		m.addIRIMapper(new OWLOntologyIRIMapper() {
			@Override
			public IRI getDocumentIRI(IRI arg0) {
				if (!mapping.containsKey(arg0.toURI())) {
					return arg0;
				}

				return IRI.create(mapping.get(arg0.toURI()));
			}
		});
		URI physicalURI = mapping.get(URI.create(ontologyURI));

		if (physicalURI == null) {
			physicalURI = URI.create(ontologyURI);
		}
		OWLOntology o;
		try {
			o = m.loadOntologyFromOntologyDocument(new File(physicalURI));
			// OWLOntology merged = new
			// OWLOntologyMerger(m).createMergedOntology(m, IRI
			// .create("http://temporary"));
			final OWLDBOntologyOutputTarget s = new OWLDBOntologyOutputTarget(
					IRI.create(dbConnection));

			m.saveOntology(o, new OWLDBOntologyFormat(),s);
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		} catch (OWLOntologyStorageException e) {
			e.printStackTrace();
		}
	}

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

}
