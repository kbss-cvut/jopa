package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;

import java.util.HashMap;
import java.util.Map;

public class JenaPersistenceFactory {

    private static final StorageConfig storage = initStorage();
    private static final Map<String, String> defaultProperties = initProperties();

    public EntityManager getEntityManager(String repositoryName, boolean cacheEnabled, Map<String, String> properties) {
        assert properties != null;
        final Map<String, String> actualProperties = new HashMap<>(defaultProperties);
        actualProperties.putAll(properties);
        return TestEnvironment
                .getPersistenceConnector("Jena" + repositoryName, storage, cacheEnabled, actualProperties);
    }

    private static StorageConfig initStorage() {
        return new JenaStorageConfig();
    }

    private static Map<String, String> initProperties() {
        final Map<String, String> map = new HashMap<>();
        map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        map.put(JOPAPersistenceProperties.LANG, "en");
        map.put(JenaOntoDriverProperties.JENA_STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        return map;
    }
}
