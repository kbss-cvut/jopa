package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.sparql.core.DatasetImpl;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.*;

public class MemoryStorageTest extends StorageTestUtil {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void initializationCreatesTransactionalInMemoryDataset() {

        final Storage storage = new MemoryStorage(StorageTestUtil.createConfiguration("urn:test"));
        storage.initialize();
        final Dataset dataset = storage.getDataset();
        assertNotNull(dataset);
        assertTrue(dataset.supportsTransactions());
    }

    @Test
    public void setDatasetReplacesStorageDataset() {
        final Storage storage = new MemoryStorage(StorageTestUtil.createConfiguration("urn:test"));
        storage.initialize();

        final Dataset newDataset = DatasetFactory.createTxnMem();
        generateTestData(newDataset);
        storage.setDataset(newDataset);
        assertEquals(newDataset, storage.getDataset());
    }

    @Test
    public void setDatasetThrowsIllegalArgumentWhenDatasetDoesNotSupportTransactions() {
        final Storage storage = new MemoryStorage(StorageTestUtil.createConfiguration("urn:test"));
        storage.initialize();

        final Dataset newDataset = new NonTransactionDataset(ModelFactory.createDefaultModel());
        assertFalse(newDataset.supportsTransactions());
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("The provided dataset does not support transactions.");
        storage.setDataset(newDataset);
    }

    private static final class NonTransactionDataset extends DatasetImpl {

        private NonTransactionDataset(Model model) {
            super(model);
        }

        @Override
        public boolean supportsTransactions() {
            return false;
        }
    }

    @Test
    public void setDatasetThrowsIllegalStateWhenDatasetIsInTransaction() {
        final Storage storage = new MemoryStorage(StorageTestUtil.createConfiguration("urn:test"));
        storage.initialize();
        storage.begin(ReadWrite.WRITE);

        thrown.expect(IllegalStateException.class);
        thrown.expectMessage("Cannot replace dataset when it is in transaction.");
        storage.setDataset(DatasetFactory.create());
    }
}