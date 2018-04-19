package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.DeleteOperationsWithInheritanceRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DeleteOperationsWithInheritanceTest extends DeleteOperationsWithInheritanceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(DeleteOperationsWithInheritanceTest.class);

    public DeleteOperationsWithInheritanceTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
