package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.DeleteOperationsMultiContextRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DeleteOperationsMultiContextTest extends DeleteOperationsMultiContextRunner {

    private static final Logger LOG = LoggerFactory.getLogger(DeleteOperationsMultiContextTest.class);

    public DeleteOperationsMultiContextTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
