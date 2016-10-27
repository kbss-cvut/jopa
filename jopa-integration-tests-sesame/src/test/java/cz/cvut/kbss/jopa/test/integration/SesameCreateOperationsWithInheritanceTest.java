package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.test.environment.SesameDataPersist;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.environment.Triple;
import cz.cvut.kbss.jopa.test.runner.CreateOperationsWithInheritanceRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;

public class SesameCreateOperationsWithInheritanceTest extends CreateOperationsWithInheritanceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(SesameCreateOperationsWithInheritanceTest.class);

    private final SesamePersistenceFactory persistenceFactory;
    private final SesameDataPersist dataPersist;

    public SesameCreateOperationsWithInheritanceTest() {
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
