package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.MultilingualAttributesTestRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MultilingualAttributesTest extends MultilingualAttributesTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(MultilingualAttributesTest.class);

    public MultilingualAttributesTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }
}
