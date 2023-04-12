package cz.cvut.kbss.jopa.test.integration.jena;

import cz.cvut.kbss.jopa.test.environment.JenaDataAccessor;
import cz.cvut.kbss.jopa.test.environment.JenaPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.EnumMappingTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EnumMappingTest extends EnumMappingTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(EnumMappingTest.class);

    public EnumMappingTest() {
        super(LOG, new JenaPersistenceFactory(), new JenaDataAccessor());
    }
}
