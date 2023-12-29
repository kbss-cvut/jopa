package cz.cvut.kbss.jopa.test.integration.rdf4j;

import cz.cvut.kbss.jopa.test.environment.Rdf4jDataAccessor;
import cz.cvut.kbss.jopa.test.environment.Rdf4jPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.ListsTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ListsTest extends ListsTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(ListsTest.class);

    public ListsTest() {
        super(LOG, new Rdf4jPersistenceFactory(), new Rdf4jDataAccessor());
    }
}
