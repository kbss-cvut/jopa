package cz.cvut.kbss.jopa.example01;

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

    private static EntityManagerFactory emf;

    private PersistenceFactory() {
        throw new AssertionError();
    }

    static {
        // Here we set up basic storage access properties - driver class, physical location of the storage
        final OntologyStorageProperties storageProperties = OntologyStorageProperties.physicalUri(
                URI.create("JOPASesameDemo")).driver("cz.cvut.kbss.ontodriver.sesame.SesameDataSource").build();
        final Map<String, String> properties = new HashMap<>();
        // View transactional changes during transaction
        properties.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY,
                Boolean.TRUE.toString());
        // Use in-memory storage if not remote or local file path specified
        properties.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE,
                Boolean.TRUE.toString());
        // Don't use Sesame inference
        properties.put(OntoDriverProperties.SESAME_USE_INFERENCE,
                Boolean.FALSE.toString());
        // Ontology language
        properties.put(OWLAPIPersistenceProperties.LANG, "en");
        // Use the new storage type (this option will not be necessary in the future)
        properties.put("storage", "new");
        // Where the entity classes are
        properties.put(OWLAPIPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.example01.model");
        // Persistence provider name
        properties.put(OWLAPIPersistenceProperties.JPA_PERSISTENCE_PROVIDER,
                OWLAPIPersistenceProvider.class.getName());
        emf = Persistence.createEntityManagerFactory("jopaExample01PU",
                storageProperties, properties);
    }

    public static EntityManager createEntityManager() {
        return emf.createEntityManager();
    }

    public static void close() {
        emf.close();
    }
}
