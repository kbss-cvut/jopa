package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.DeleteOperationsRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DeleteOperationsTest extends DeleteOperationsRunner {

    private static final Logger LOG = LoggerFactory.getLogger(DeleteOperationsTest.class);

    public DeleteOperationsTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
