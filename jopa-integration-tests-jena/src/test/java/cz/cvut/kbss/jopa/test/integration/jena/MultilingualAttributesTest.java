package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.CreateOperationsRunner;
import cz.cvut.kbss.jopa.test.runner.MultilingualAttributesTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MultilingualAttributesTest extends MultilingualAttributesTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(CreateOperationsRunner.class);

    public MultilingualAttributesTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
