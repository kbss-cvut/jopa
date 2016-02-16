package cz.cvut.kbss.jopa.test.integration.environment;

import com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.test.environment.OwlapiStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

import java.util.HashMap;
import java.util.Map;

public class OwlapiPersistenceFactory {

    private static final StorageConfig storage = initStorage();
    private static final Map<String, String> defaultProperties = initProperties();

    public EntityManager getEntityManager(String repositoryName, boolean cacheEnabled, Map<String, String> properties) {
        assert properties != null;
        final Map<String, String> actualProperties = new HashMap<>(defaultProperties);
        actualProperties.putAll(properties);
        return TestEnvironment
                .getPersistenceConnector("Owlapi" + repositoryName, storage, cacheEnabled, actualProperties);
    }

    private static StorageConfig initStorage() {
        return new OwlapiStorageConfig();
    }

    private static Map<String, String> initProperties() {
        final Map<String, String> map = new HashMap<>();
        map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        map.put(JOPAPersistenceProperties.LANG, "en");
        map.put(OntoDriverProperties.OWLAPI_REASONER_FACTORY_CLASS, PelletReasonerFactory.class.getName());
        return map;
    }
}
