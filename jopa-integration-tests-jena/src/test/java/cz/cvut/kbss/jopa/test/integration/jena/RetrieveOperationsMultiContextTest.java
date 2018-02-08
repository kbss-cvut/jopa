package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.RetrieveOperationsMultiContextRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RetrieveOperationsMultiContextTest extends RetrieveOperationsMultiContextRunner {

    private static final Logger LOG = LoggerFactory.getLogger(RetrieveOperationsMultiContextTest.class);

    public RetrieveOperationsMultiContextTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
