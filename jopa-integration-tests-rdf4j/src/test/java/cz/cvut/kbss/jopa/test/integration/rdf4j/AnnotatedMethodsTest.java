package cz.cvut.kbss.jopa.test.integration.rdf4j;

import cz.cvut.kbss.jopa.test.environment.Rdf4jDataAccessor;
import cz.cvut.kbss.jopa.test.environment.Rdf4jPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.AnnotatedMethodsTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AnnotatedMethodsTest extends AnnotatedMethodsTestRunner {
    private static final Logger LOG = LoggerFactory.getLogger(AnnotatedMethodsTest.class);

    public AnnotatedMethodsTest() {
        super(LOG, new Rdf4jPersistenceFactory(), new Rdf4jDataAccessor());
    }
}
