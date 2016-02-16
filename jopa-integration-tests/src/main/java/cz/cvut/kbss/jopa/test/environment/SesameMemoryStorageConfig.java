package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.owlapi.JOPAPersistenceProperties;

import java.util.HashMap;
import java.util.Map;

/**
 * Persistent storage configuration for Sesame in-memory store.
 *
 * @author ledvima1
 */
public class SesameMemoryStorageConfig extends StorageConfig {

    protected static final OntologyConnectorType TYPE = OntologyConnectorType.SESAME;

    public SesameMemoryStorageConfig() {
        super();
    }

    @Override
    public Map<String, String> createStorageConfiguration(int index) {
        assert index >= 0;
        assert name != null;

        final String base = name + TYPE.toString() + index;

        final Map<String, String> config = new HashMap<>();
        config.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, TYPE.getDriverClass());
        config.put(JOPAPersistenceProperties.ONTOLOGY_URI_KEY, TestEnvironment.IRI_BASE + base);
        config.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY, base);
        return config;
    }
}
