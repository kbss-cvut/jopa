package cz.cvut.kbss.jopa.test.query.jena;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.TypedQueryRunner;
import cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties;
import org.junit.After;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

public class TypedQueryTest extends TypedQueryRunner {

    private static final Logger LOG = LoggerFactory.getLogger(QueryTest.class);

    private static EntityManager em;

    public TypedQueryTest() {
        super(LOG);
    }

    @Before
    public void setUp() {
        final JenaPersistenceFactory persistenceFactory = new JenaPersistenceFactory();
        final Map<String, String> properties = new HashMap<>();
        properties.put(JenaOntoDriverProperties.JENA_STORAGE_TYPE, JenaOntoDriverProperties.IN_MEMORY);
        properties.put(JenaOntoDriverProperties.JENA_TREAT_DEFAULT_GRAPH_AS_UNION, Boolean.toString(true));
        em = persistenceFactory.getEntityManager("SPARQLTypedQueryTests", false, properties);
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
    }

    @After
    public void tearDown() {
        em.close();
        em.getEntityManagerFactory().close();
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }
}
