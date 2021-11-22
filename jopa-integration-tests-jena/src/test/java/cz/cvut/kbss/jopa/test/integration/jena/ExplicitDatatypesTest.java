package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.ExplicitDatatypesRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExplicitDatatypesTest extends ExplicitDatatypesRunner {

    private static final Logger LOG = LoggerFactory.getLogger(ExplicitDatatypesTest.class);

    public ExplicitDatatypesTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
