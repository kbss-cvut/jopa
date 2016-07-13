package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.EntityManagerFactoryImpl;
import cz.cvut.kbss.jopa.test.integration.environment.PersistenceFactory;
import cz.cvut.kbss.jopa.test.integration.environment.TestDataSource;
import org.junit.After;
import org.junit.Before;

import java.util.Collections;

public class IntegrationTestBase {

    EntityManagerFactory emf;
    EntityManager em;

    @Before
    public void setUp() throws Exception {
        this.emf = PersistenceFactory.initPersistence(Collections.emptyMap());
        this.em = emf.createEntityManager();
    }

    @After
    public void tearDown() throws Exception {
        em.close();
        emf.close();
    }

    TestDataSource getDataSource() {
        return ((EntityManagerFactoryImpl) emf).getServerSession().unwrap(TestDataSource.class);
    }
}
