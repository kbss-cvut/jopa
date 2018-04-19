package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.UpdateOperationsMultiContextRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UpdateOperationsMultiContextTest extends UpdateOperationsMultiContextRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateOperationsMultiContextTest.class);

    public UpdateOperationsMultiContextTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
