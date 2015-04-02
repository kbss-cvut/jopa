package cz.cvut.kbss.jopa.test.utils;

import java.io.File;
import java.net.URI;

import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

/**
 * Persistent storage configuration for OWLAPI accessed single-file storage.
 * 
 * @author ledvima1
 * 
 */
public class OwlapiStorageConfig extends StorageConfig {

	protected static final OntologyConnectorType TYPE = OntologyConnectorType.OWLAPI;

	public OwlapiStorageConfig() {
		super();
	}

	@Override
	public OntologyStorageProperties createStorageProperties(int index) {
		assert directory != null : "directory is not set";
		assert name != null;
		assert index >= 0;

		String base = name + TYPE.toString() + index;
		final URI ontoUri = URI.create(TestEnvironment.IRI_BASE + base);
		final File url = new File(directory + File.separator + base + ".owl");
		final URI physicalUri = url.toURI();
		TestEnvironment.removeOldTestFiles(url);

		return OntologyStorageProperties.ontologyUri(ontoUri).physicalUri(physicalUri).connectorType(TYPE).build();
	}

}
