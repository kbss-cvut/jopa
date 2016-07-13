package cz.cvut.kbss.jopa.test.integration.environment;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProvider;

import java.util.HashMap;
import java.util.Map;

public class PersistenceFactory {

    public static EntityManagerFactory initPersistence(Map<String, String> properties) {
        final Map<String, String> props = new HashMap<>();
        props.put(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.test");
        props.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, TestDataSource.class.getCanonicalName());
        props.put(JOPAPersistenceProperties.JPA_PERSISTENCE_PROVIDER, JOPAPersistenceProvider.class.getName());
        props.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY, "TestOntology");
        props.putAll(properties);
        return Persistence.createEntityManagerFactory("testPU", props);
    }
}
