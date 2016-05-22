package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.integration.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.PersistenceUnitTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.Map;

public class OwlapiPersistenceUnitTest extends PersistenceUnitTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(OwlapiPersistenceUnitTest.class);

    private OwlapiPersistenceFactory persistenceFactory;

    public OwlapiPersistenceUnitTest() {
        super(LOG);
        this.persistenceFactory = new OwlapiPersistenceFactory();
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
