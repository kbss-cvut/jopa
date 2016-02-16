package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.owlapi.JOPAPersistenceProperties;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

/**
 * Persistent storage configuration for Jena accessed single-file storage.
 *
 * @author ledvima1
 */
public class JenaStorageConfig extends StorageConfig {

    protected static final OntologyConnectorType TYPE = OntologyConnectorType.JENA;

    public JenaStorageConfig() {
        super();
    }

    @Override
    public Map<String, String> createStorageConfiguration(int index) {
        assert index >= 0;
        assert name != null;
        assert directory != null;

        String base = name + TYPE.toString() + index;
        final File url = new File(directory + File.separator + base + ".owl");
        TestEnvironment.removeOldTestFiles(url);

        final Map<String, String> config = new HashMap<>();
        config.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, TYPE.getDriverClass());
        config.put(JOPAPersistenceProperties.ONTOLOGY_URI_KEY, TestEnvironment.IRI_BASE + base);
        config.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY, url.toURI().toString());
        return config;

    }

}
