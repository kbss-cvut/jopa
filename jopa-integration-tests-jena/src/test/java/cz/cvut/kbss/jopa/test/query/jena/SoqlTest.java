package cz.cvut.kbss.jopa.test.query.jena;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.SoqlRunner;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

public class SoqlTest extends SoqlRunner {

    private static final Logger LOG = LoggerFactory.getLogger(SoqlTest.class);

    private static EntityManager em;

    public SoqlTest() {
        super(LOG);
    }

    @BeforeEach
    void setUp() {
        final JenaPersistenceFactory persistenceFactory = new JenaPersistenceFactory();
        final Map<String, String> properties = new HashMap<>();
        properties.put(JenaOntoDriverProperties.JENA_STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        properties.put(JenaOntoDriverProperties.JENA_TREAT_DEFAULT_GRAPH_AS_UNION, Boolean.toString(true));
        em = persistenceFactory.getEntityManager("SOQLTests", false, properties);
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
    }

    @AfterEach
    void tearDown() {
        em.close();
        em.getEntityManagerFactory().close();
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }
}
