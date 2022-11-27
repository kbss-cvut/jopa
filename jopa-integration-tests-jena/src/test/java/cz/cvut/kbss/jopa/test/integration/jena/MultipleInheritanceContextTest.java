package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.MultipleInheritanceTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MultipleInheritanceContextTest  extends MultipleInheritanceTestRunner {
    private static final Logger LOG = LoggerFactory.getLogger(MultipleInheritanceContextTest.class);

    public MultipleInheritanceContextTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
