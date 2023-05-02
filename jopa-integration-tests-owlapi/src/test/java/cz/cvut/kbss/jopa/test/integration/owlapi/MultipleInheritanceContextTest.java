package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.MultipleInheritanceTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MultipleInheritanceContextTest extends MultipleInheritanceTestRunner {
    private static final Logger LOG = LoggerFactory.getLogger(MultipleInheritanceContextTest.class);

    public MultipleInheritanceContextTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }
}
