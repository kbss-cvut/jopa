package cz.cvut.kbss.jopa.test.integration.rdf4j;

import cz.cvut.kbss.jopa.test.environment.Rdf4jDataAccessor;
import cz.cvut.kbss.jopa.test.environment.Rdf4jPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.MultipleInheritanceTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MultipleInheritanceContextTest extends MultipleInheritanceTestRunner {
    private static final Logger LOG = LoggerFactory.getLogger(MultipleInheritanceContextTest.class);

    public MultipleInheritanceContextTest() {
        super(LOG, new Rdf4jPersistenceFactory(), new Rdf4jDataAccessor());
    }
}
