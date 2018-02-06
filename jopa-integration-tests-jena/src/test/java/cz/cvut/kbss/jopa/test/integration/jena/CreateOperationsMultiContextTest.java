package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Triple;
import cz.cvut.kbss.jopa.test.runner.CreateOperationsMultiContextRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.Map;

public class CreateOperationsMultiContextTest extends CreateOperationsMultiContextRunner {

    private static final Logger LOG = LoggerFactory.getLogger(CreateOperationsMultiContextTest.class);

    private final JenaPersistenceFactory persistenceFactory;
    private final JenaDataAccessor dataAccessor;

    public CreateOperationsMultiContextTest() {
        super(LOG);
        this.persistenceFactory = new JenaPersistenceFactory();
        this.dataAccessor = new JenaDataAccessor();
    }

    @Override
    protected EntityManager getEntityManager(String repositoryName, boolean cacheEnabled,
                                             Map<String, String> properties) {
        return persistenceFactory.getEntityManager(repositoryName, cacheEnabled, properties);
    }

    @Override
    protected void persistTestData(Collection<Triple> data, EntityManager em) {
        dataAccessor.persistTestData(data, em);
    }

    @Override
    protected void verifyStatementsPresent(Collection<Triple> expected, EntityManager em) {
        dataAccessor.verifyDataPresence(expected, em);
    }
}
