package cz.cvut.kbss.jopa.test.utils;

import java.net.URI;

import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.OwldbOntologyStorageProperties;

/**
 * Persistent storage configuration for OWLAPI accessed OWLDB storage.
 * 
 * @author ledvima1
 * 
 */
public class OwldbStorageConfig extends OwlapiStorageConfig {

	public OwldbStorageConfig() {
		super();
	}

	@Override
	public OntologyStorageProperties createStorageProperties(int index) {
		assert index >= 0;
		assert name != null;

		String base = name + TYPE.toString() + index;
		final URI ontoUri = URI.create(TestEnvironment.IRI_BASE + base);
		URI physicalUri = URI.create(TestEnvironment.DB_URI);

		return OwldbOntologyStorageProperties.ontologyUri(ontoUri).physicalUri(physicalUri)
				.connectorType(TYPE).username(TestEnvironment.DB_USERNAME)
				.password(TestEnvironment.DB_PASSWORD).jdbcDriverClass(TestEnvironment.DB_DRIVER)
				.build();
	}

}
