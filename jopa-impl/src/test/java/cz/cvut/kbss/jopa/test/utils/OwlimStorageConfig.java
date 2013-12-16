package cz.cvut.kbss.jopa.test.utils;

import java.io.File;
import java.net.URI;

import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.OwlimOntologyStorageProperties;

/**
 * Persistent storage configuration for Sesame accessed OWLIM storage.
 * 
 * @author ledvima1
 * 
 */
public class OwlimStorageConfig extends StorageConfig {

	private static final OntologyConnectorType TYPE = OntologyConnectorType.OWLIM;
	private static final String CONFIG_FILE = "owlimConfig.ttl";

	public OwlimStorageConfig() {
		super();
	}

	@Override
	public OntologyStorageProperties createStorageProperties(int index) {
		assert index >= 0;
		assert name != null;
		assert directory != null;

		String base = name + TYPE.toString() + index;
		final URI ontoUri = URI.create(TestEnvironment.IRI_BASE + base);
		final File url = new File(directory + File.separator + base);
		final URI physicalUri = url.toURI();
		TestEnvironment.removeOldTestFiles(url);
		boolean res = url.mkdirs();
		assert res;
		final String configFile = directory + File.separator + CONFIG_FILE;

		return OwlimOntologyStorageProperties.ontologyUri(ontoUri).physicalUri(physicalUri)
				.owlimConfigFile(configFile).connectorType(TYPE).build();
	}

}
