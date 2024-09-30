package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.RdfContainersTestRunner;
import org.junit.jupiter.api.Disabled;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Disabled
public class RdfContainersTest extends RdfContainersTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(RdfContainersTest.class);

    public RdfContainersTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
