package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.integration.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.RetrieveOperationsRunner;

import java.util.Collections;
import java.util.Map;
import java.util.logging.Logger;

public class OwlapiRetrieveOperationsTest extends RetrieveOperationsRunner {

    private static final Logger LOG = Logger.getLogger(OwlapiRetrieveOperationsTest.class.getName());

    private OwlapiPersistenceFactory persistenceFactory;

    public OwlapiRetrieveOperationsTest() {
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
