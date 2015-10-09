package cz.cvut.kbss.jopa.test.query;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.query.runner.TypedQueryRunner;
import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import org.junit.AfterClass;
import org.junit.BeforeClass;

import java.util.Collections;
import java.util.logging.Logger;

public class SesameTypedQueryTests extends TypedQueryRunner {

    private static final Logger LOG = Logger.getLogger(SesameTypedQueryTests.class.getName());

    private static EntityManager em;

    public SesameTypedQueryTests() {
        super(LOG);
    }

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        final SesamePersistenceFactory persistenceFactory = new SesamePersistenceFactory();
        em = persistenceFactory.getEntityManager("SPARQLTypedQueryTests", false,
                Collections.singletonMap(OntoDriverProperties.SESAME_USE_INFERENCE, "true"));
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
}
