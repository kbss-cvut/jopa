package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.DeleteOperationsMultiContextRunner;

import java.util.Collections;
import java.util.Map;
import java.util.logging.Logger;

public class SesameDeleteOperationsMultiContextTest extends DeleteOperationsMultiContextRunner {

    private static final Logger LOG = Logger
            .getLogger(SesameDeleteOperationsMultiContextTest.class.getName());

    private SesamePersistenceFactory persistenceFactory;

    public SesameDeleteOperationsMultiContextTest() {
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
