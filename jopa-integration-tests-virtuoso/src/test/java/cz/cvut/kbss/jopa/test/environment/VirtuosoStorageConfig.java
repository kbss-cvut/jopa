package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;

import java.util.HashMap;
import java.util.Map;

public class VirtuosoStorageConfig extends StorageConfig {

    private static final OntologyConnectorType TYPE = OntologyConnectorType.VIRTUOSO;

    @Override
    public Map<String, String> createStorageConfiguration() {
        assert name != null;

        final String base = name + TYPE;

        final Map<String, String> config = new HashMap<>();
        config.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, TYPE.getDriverClass());
        config.put(JOPAPersistenceProperties.ONTOLOGY_URI_KEY, TestEnvironment.IRI_BASE + base);
        final String host = System.getProperty("virtuoso.host", "");
        final String port = System.getProperty("virtuoso.port", "");
        if (host.isBlank() || port.isBlank()) {
            throw new IllegalStateException("Missing host or port setting for Virtuoso");
        }
        config.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY, "jdbc:virtuoso://" + host + ":" + port + "/log_enable=0");
        if (System.getProperty("virtuoso.username") != null) {
            config.put(OntoDriverProperties.DATA_SOURCE_USERNAME, System.getProperty("virtuoso.username"));
        }
        if (System.getProperty("virtuoso.password") != null) {
            config.put(OntoDriverProperties.DATA_SOURCE_PASSWORD, System.getProperty("virtuoso.password"));
        }
        return config;
    }
}
