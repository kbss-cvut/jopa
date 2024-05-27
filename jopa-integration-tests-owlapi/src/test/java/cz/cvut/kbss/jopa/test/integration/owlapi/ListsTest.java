package cz.cvut.kbss.jopa.test.integration.owlapi;

import cz.cvut.kbss.jopa.test.environment.OwlapiDataAccessor;
import cz.cvut.kbss.jopa.test.environment.OwlapiPersistenceFactory;
import cz.cvut.kbss.jopa.test.runner.ListsTestRunner;
import org.junit.jupiter.api.Disabled;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ListsTest extends ListsTestRunner {

    private static final Logger LOG = LoggerFactory.getLogger(ListsTest.class);

    public ListsTest() {
        super(LOG, new OwlapiPersistenceFactory(), new OwlapiDataAccessor());
    }

    // RDF lists are not supported by the OWL API driver
    @Disabled
    @Override
    public void persistSavesRdfCollectionTerminatedByNil() {
        // Do nothing
    }

    // RDF lists are not supported by the OWL API driver
    @Disabled
    @Override
    public void updateUpdatesRdfCollectionTerminatedByNilAndMaintainsCorrectListEnding() {
        // Do nothing
    }
}
