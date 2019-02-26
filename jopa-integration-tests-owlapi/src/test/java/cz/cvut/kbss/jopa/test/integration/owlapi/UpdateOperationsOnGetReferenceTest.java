package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.UpdateOperationsOnGetReferenceRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UpdateOperationsOnGetReferenceTest extends UpdateOperationsOnGetReferenceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateOperationsOnGetReferenceTest.class);

    public UpdateOperationsOnGetReferenceTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }
}
