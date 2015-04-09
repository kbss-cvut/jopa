package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

import java.io.File;
import java.net.URI;

/**
 * Persistent storage configuration for Sesame native triple store.
 * 
 * @author ledvima1
 * 
 */
public class SesameNativeStorageConfig extends SesameMemoryStorageConfig {

	public SesameNativeStorageConfig() {
		super();
	}

	@Override
	public OntologyStorageProperties createStorageProperties(int index) {
		assert index >= 0;
		assert name != null;
		assert directory != null;

		String base = name + TYPE.toString() + index;
		final URI ontoUri = URI.create(TestEnvironment.IRI_BASE + base);
		final File url = new File(directory + File.separator + "openrdf-sesame" + File.separator
				+ "repositories" + File.separator + base);
		final URI physicalUri = url.toURI();
		TestEnvironment.removeOldTestFiles(url);
		boolean res = url.mkdirs();
		assert res;

		return OntologyStorageProperties.ontologyUri(ontoUri).physicalUri(physicalUri).connectorType(TYPE).build();
	}

}
