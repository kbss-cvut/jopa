package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.integration.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.DeleteOperationsRunner;

import java.util.Collections;
import java.util.Map;
import java.util.logging.Logger;

public class OwlapiDeleteOperationsTest extends DeleteOperationsRunner {

    private static final Logger LOG = Logger.getLogger(OwlapiDeleteOperationsTest.class.getName());

    private OwlapiPersistenceFactory persistenceFactory;

    public OwlapiDeleteOperationsTest() {
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
