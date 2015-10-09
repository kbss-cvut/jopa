package cz.cvut.kbss.jopa.test.query.owlapi;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.integration.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.TypedQueryRunner;
import org.junit.AfterClass;
import org.junit.BeforeClass;

import java.util.Collections;
import java.util.logging.Logger;

public class OwlapiTypedQueryTest extends TypedQueryRunner {

    private static final Logger LOG = Logger.getLogger(OwlapiTypedQueryTest.class.getName());

    private static EntityManager em;

    public OwlapiTypedQueryTest() {
        super(LOG);
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        final OwlapiPersistenceFactory persistenceFactory = new OwlapiPersistenceFactory();
        em = persistenceFactory.getEntityManager("SPARQLTypedQueryTests", false, Collections.emptyMap());
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
    }

    @AfterClass
    public static void tearDownAfterClass() throws Exception {
        em.close();
        em.getEntityManagerFactory().close();
    }
}
