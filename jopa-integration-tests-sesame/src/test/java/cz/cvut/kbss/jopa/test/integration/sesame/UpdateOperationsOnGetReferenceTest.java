package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.test.environment.SesameDataAccessor;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.UpdateOperationsOnGetReferenceRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UpdateOperationsOnGetReferenceTest extends UpdateOperationsOnGetReferenceRunner {

    private static final Logger LOG = LoggerFactory.getLogger(UpdateOperationsOnGetReferenceTest.class);

    public UpdateOperationsOnGetReferenceTest() {
        super(LOG, new SesamePersistenceFactory(), new SesameDataAccessor());
    }
}
