package cz.cvut.kbss.jopa.test.query.owlapi;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.integration.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.QueryRunner;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;

import java.util.Collections;
import java.util.logging.Logger;

public class OwlapiQueryTest extends QueryRunner {

    private static final Logger LOG = Logger.getLogger(OwlapiQueryTest.class.getName());

    private static EntityManager em;

    public OwlapiQueryTest() {
        super(LOG);
    }

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        final OwlapiPersistenceFactory persistenceFactory = new OwlapiPersistenceFactory();
        em = persistenceFactory.getEntityManager("SPARQLQueryTests", false, Collections.emptyMap());
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

    @Ignore
    @Override
    public void testSelectWithOptionalReturnsNullInUnfilledColumns() throws Exception {
        // OWL2Query does not support OPTIONAL pattern
    }
}
