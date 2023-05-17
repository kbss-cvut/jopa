package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.AnnotatedMethodsTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AnnotatedMethodsTest  extends AnnotatedMethodsTestRunner {
    private static final Logger LOG = LoggerFactory.getLogger(AnnotatedMethodsTest.class);

    public AnnotatedMethodsTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }
}
