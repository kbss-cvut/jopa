package cz.cvut.kbss.jopa.test.integration.rdf4j;

import cz.cvut.kbss.jopa.test.environment.Rdf4jDataAccessor;
import cz.cvut.kbss.jopa.test.environment.Rdf4jPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.FetchGraphRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FetchGraphTest extends FetchGraphRunner {

    private static final Logger LOG = LoggerFactory.getLogger(FetchGraphTest.class);

    public FetchGraphTest() {
        super(LOG, new Rdf4jPersistenceFactory(), new Rdf4jDataAccessor());
    }
}
