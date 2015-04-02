package cz.cvut.kbss.jopa.test.utils;

import java.net.URI;

import cz.cvut.kbss.jopa.test.TestEnvironment;
import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

/**
 * Persistent storage configuration for Sesame in-memory store.
 * 
 * @author ledvima1
 * 
 */
public class SesameMemoryStorageConfig extends StorageConfig {

	protected static final OntologyConnectorType TYPE = OntologyConnectorType.SESAME;

	public SesameMemoryStorageConfig() {
		super();
	}

	@Override
	public OntologyStorageProperties createStorageProperties(int index) {
		assert index >= 0;
		assert name != null;

		final String base = name + TYPE.toString() + index;
		final URI ontoUri = URI.create(TestEnvironment.IRI_BASE + base);
		final URI physicalUri = URI.create(base);

		return OntologyStorageProperties.ontologyUri(ontoUri).physicalUri(physicalUri).connectorType(TYPE).build();
	}

}
