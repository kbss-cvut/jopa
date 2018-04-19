package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.UpdateOperationsWithInheritanceRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UpdateOperationsWithInheritanceTest extends UpdateOperationsWithInheritanceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateOperationsWithInheritanceTest.class);

    public UpdateOperationsWithInheritanceTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
