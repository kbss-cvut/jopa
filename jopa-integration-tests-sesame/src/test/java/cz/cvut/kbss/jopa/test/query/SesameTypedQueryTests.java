package cz.cvut.kbss.jopa.test.query;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.SesameMemoryStorageConfig;
import cz.cvut.kbss.jopa.test.environment.StorageConfig;
import cz.cvut.kbss.jopa.test.environment.TestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.TypedQueryRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import org.junit.AfterClass;
import org.junit.BeforeClass;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class SesameTypedQueryTests extends TypedQueryRunner {

    private static final Logger LOG = Logger.getLogger(SesameTypedQueryTests.class.getName());

    private static StorageConfig storage = initStorage();
    private static Map<String, String> properties = initProperties();

    private static EntityManager em;

    public SesameTypedQueryTests() {
        super(LOG);
    }

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        em = TestEnvironment.getPersistenceConnector("SesameMemStoreSPARQLTypedQueryTests", storage, false, properties);
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
    }

    @AfterClass
    public static void tearDownAfterClass() throws Exception {
        em.close();
        em.getEntityManagerFactory().close();
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }

    private static StorageConfig initStorage() {
        return new SesameMemoryStorageConfig();
    }

    private static Map<String, String> initProperties() {
        final Map<String, String> map = new HashMap<>();
        map.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        map.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        map.put("storage", "new");
        return map;
    }
}
