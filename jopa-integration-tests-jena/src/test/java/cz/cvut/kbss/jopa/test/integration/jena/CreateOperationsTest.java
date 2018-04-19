package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.CreateOperationsRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CreateOperationsTest extends CreateOperationsRunner {

    private static final Logger LOG = LoggerFactory.getLogger(CreateOperationsRunner.class);

    public CreateOperationsTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
