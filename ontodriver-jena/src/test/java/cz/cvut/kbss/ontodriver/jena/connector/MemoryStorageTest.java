package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.junit.Test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class MemoryStorageTest extends StorageTestBase {

    @Test
    public void initializationCreatesTransactionalInMemoryDataset() {
        final Storage storage = new MemoryStorage();
        storage.initialize();
        final Dataset dataset = storage.getDataset();
        assertNotNull(dataset);
        assertTrue(dataset.supportsTransactions());
    }
}