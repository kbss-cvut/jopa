package cz.cvut.kbss.jopa.test.query.sesame;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.query.QueryTestEnvironment;
import cz.cvut.kbss.jopa.test.query.runner.SoqlRunner;
import cz.cvut.kbss.ontodriver.sesame.config.SesameOntoDriverProperties;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;

public class SoqlTest extends SoqlRunner {

    private static final Logger LOG = LoggerFactory.getLogger(SoqlTest.class);

    private static EntityManager em;

    SoqlTest() {
        super(LOG);
    }

    @BeforeAll
    static void setUpBeforeClass() {
        final SesamePersistenceFactory persistenceFactory = new SesamePersistenceFactory();
        em = persistenceFactory.getEntityManager("SOQLTests", false,
                Collections.singletonMap(SesameOntoDriverProperties.SESAME_USE_INFERENCE, "true"));
        QueryTestEnvironment.generateTestData(em);
        em.clear();
        em.getEntityManagerFactory().getCache().evictAll();
    }

    @BeforeEach
    void setUp() {
        em.clear();
    }

    @AfterAll
    static void tearDownAfterClass() {
        em.close();
        em.getEntityManagerFactory().close();
    }

    @Override
    protected EntityManager getEntityManager() {
        return em;
    }
}
