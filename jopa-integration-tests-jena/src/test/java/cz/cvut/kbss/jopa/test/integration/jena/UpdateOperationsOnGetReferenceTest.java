package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.UpdateOperationsOnGetReferenceRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UpdateOperationsOnGetReferenceTest extends UpdateOperationsOnGetReferenceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateOperationsOnGetReferenceTest.class);

    public UpdateOperationsOnGetReferenceTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
