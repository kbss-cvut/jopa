package cz.cvut.kbss.jopa.example01;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProvider;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;

import java.util.HashMap;
import java.util.Map;

public class PersistenceFactory {

    private static boolean initialized = false;

    private static EntityManagerFactory emf;

    private PersistenceFactory() {
        throw new AssertionError();
    }

    public static void init(Map<String, String> properties) {
        final Map<String, String> props = new HashMap<>();
        // Here we set up basic storage access properties - driver class, physical location of the storage
        props.put(JOPAPersistenceProperties.ONTOLOGY_PHYSICAL_URI_KEY, "JOPASesameDemo");
        props.put(JOPAPersistenceProperties.DATA_SOURCE_CLASS, "cz.cvut.kbss.ontodriver.sesame.SesameDataSource");
        // View transactional changes during transaction
        props.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        // Use in-memory storage if not remote or local file path specified
        props.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        // Don't use Sesame inference
        props.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
        // Ontology language
        props.put(JOPAPersistenceProperties.LANG, "en");
        if (properties != null) {
            props.putAll(properties);
        }
        // Persistence provider name
        props.put(JOPAPersistenceProperties.JPA_PERSISTENCE_PROVIDER, JOPAPersistenceProvider.class.getName());

        emf = Persistence.createEntityManagerFactory("jopaExample01PU", props);
        initialized = true;
    }

    public static EntityManager createEntityManager() {
        if (!initialized) {
            throw new IllegalStateException("Factory has not been initialized.");
        }
        return emf.createEntityManager();
    }

    public static void close() {
        emf.close();
    }
}
