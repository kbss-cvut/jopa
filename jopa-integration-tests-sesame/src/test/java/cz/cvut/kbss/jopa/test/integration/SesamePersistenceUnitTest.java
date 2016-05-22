package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.PersistenceUnitTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.Map;

public class SesamePersistenceUnitTest extends PersistenceUnitTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(SesamePersistenceUnitTest.class);

    private SesamePersistenceFactory persistenceFactory;

    public SesamePersistenceUnitTest() {
        super(LOG);
        this.persistenceFactory = new SesamePersistenceFactory();
    }

    @Override
    protected EntityManager getEntityManager(String repositoryName, boolean cacheEnabled) {
        return getEntityManager(repositoryName, cacheEnabled, Collections.emptyMap());
    }

    @Override
    protected EntityManager getEntityManager(String repositoryName, boolean cacheEnabled,
                                             Map<String, String> properties) {
        return persistenceFactory.getEntityManager(repositoryName, cacheEnabled, properties);
    }
}
