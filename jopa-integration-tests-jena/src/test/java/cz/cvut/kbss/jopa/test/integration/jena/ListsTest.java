package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.ListsTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ListsTest extends ListsTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(ListsTest.class);

    public ListsTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
