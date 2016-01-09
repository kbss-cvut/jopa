package cz.cvut.kbss.jopa.example03;

import cz.cvut.kbss.jopa.Persistence;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProvider;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

public class PersistenceFactory {

    private static boolean initialized = false;

    private static EntityManagerFactory emf;

    private PersistenceFactory() {
        throw new AssertionError();
    }

    public static void init(String repoPath) {
        // Here we set up basic storage access properties - driver class, physical location of the storage
        System.out.println("Using repository path: " + repoPath);
        final OntologyStorageProperties storageProperties = OntologyStorageProperties.physicalUri(
                URI.create(repoPath)).driver("cz.cvut.kbss.ontodriver.sesame.SesameDataSource").build();
        final Map<String, String> props = new HashMap<>();
        // View transactional changes during transaction
        props.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        // Don't use Sesame inference
        props.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
        // Where to look for entities
        props.put(OWLAPIPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.example03.model");
        // Ontology language
        props.put(OWLAPIPersistenceProperties.LANG, "en");
        // Persistence provider name
        props.put(OWLAPIPersistenceProperties.JPA_PERSISTENCE_PROVIDER, OWLAPIPersistenceProvider.class.getName());

        emf = Persistence.createEntityManagerFactory("jopaExample03PU", storageProperties, props);
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
