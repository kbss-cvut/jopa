package cz.cvut.kbss.jopa.test.integration.sesame;

import cz.cvut.kbss.jopa.test.environment.SesameDataAccessor;
import cz.cvut.kbss.jopa.test.environment.SesamePersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.MultilingualAttributesTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MultilingualAttributesTest extends MultilingualAttributesTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(CreateOperationsTest.class);

    public MultilingualAttributesTest() {
        super(LOG, new SesamePersistenceFactory(), new SesameDataAccessor());
    }
}
