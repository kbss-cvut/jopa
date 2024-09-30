package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.RdfContainersTestRunner;
import org.junit.jupiter.api.Disabled;
import org.slf4j.Logger;

@Disabled
public class RdfContainersTest extends RdfContainersTestRunner {

    private static final Logger LOG = org.slf4j.LoggerFactory.getLogger(RdfContainersTest.class);

    public RdfContainersTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }
}
