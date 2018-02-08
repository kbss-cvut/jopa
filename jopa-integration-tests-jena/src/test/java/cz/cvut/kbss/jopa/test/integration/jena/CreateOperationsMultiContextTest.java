package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.CreateOperationsMultiContextRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CreateOperationsMultiContextTest extends CreateOperationsMultiContextRunner {

    private static final Logger LOG = LoggerFactory.getLogger(CreateOperationsMultiContextTest.class);

    public CreateOperationsMultiContextTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
