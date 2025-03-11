package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.test.environment.VirtuosoDataAccessor;
import cz.cvut.kbss.jopa.test.environment.VirtuosoPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.RdfContainersTestRunner;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.condition.EnabledIfSystemProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@EnabledIfSystemProperty(named = "virtuoso.host", matches = ".+")
@EnabledIfSystemProperty(named = "virtuoso.port", matches = ".+")
public class RdfContainersTest extends RdfContainersTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(RdfContainersTest.class);

    public RdfContainersTest() {
        super(LOG, new VirtuosoPersistenceFactory(), new VirtuosoDataAccessor());
    }

    @AfterEach
    public void tearDown() {
        ((VirtuosoDataAccessor) dataAccessor).clearRepository(em);
        super.tearDown();
    }
}
