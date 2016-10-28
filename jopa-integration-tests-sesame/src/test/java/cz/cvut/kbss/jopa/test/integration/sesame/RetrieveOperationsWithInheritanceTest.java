package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.SesameDataPersist;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Triple;
import cz.cvut.kbss.jopa.test.runner.RetrieveOperationsWithInheritanceRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

public class RetrieveOperationsWithInheritanceTest extends RetrieveOperationsWithInheritanceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(RetrieveOperationsWithInheritanceTest.class);

    private final SesamePersistenceFactory persistenceFactory;
    private final SesameDataPersist dataPersist;

    public RetrieveOperationsWithInheritanceTest() {
        super(LOG);
        this.persistenceFactory = new SesamePersistenceFactory();
        this.dataPersist = new SesameDataPersist();
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

    @Override
    protected void persistTestData(Collection<Triple> data, EntityManager em) throws Exception {
        dataPersist.persistTestData(data, em);
    }
}
