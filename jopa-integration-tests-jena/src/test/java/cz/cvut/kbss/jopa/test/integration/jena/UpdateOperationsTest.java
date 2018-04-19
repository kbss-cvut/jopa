package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.UpdateOperationsRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UpdateOperationsTest extends UpdateOperationsRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateOperationsTest.class);

    public UpdateOperationsTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
