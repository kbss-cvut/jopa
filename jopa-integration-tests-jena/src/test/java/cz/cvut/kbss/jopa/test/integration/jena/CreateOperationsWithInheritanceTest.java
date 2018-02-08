package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.CreateOperationsWithInheritanceRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CreateOperationsWithInheritanceTest extends CreateOperationsWithInheritanceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(CreateOperationsWithInheritanceTest.class);

    public CreateOperationsWithInheritanceTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
