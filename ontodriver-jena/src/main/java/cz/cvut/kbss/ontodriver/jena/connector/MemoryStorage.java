package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;

import java.util.Objects;

/**
 * Storage accessor using an in-memory dataset.
 */
class MemoryStorage extends Storage {

    MemoryStorage(DriverConfiguration configuration) {
        super(configuration);
    }

    @Override
    void initialize() {
        this.dataset = DatasetFactory.createTxnMem();
    }

    @Override
    void setDataset(Dataset dataset) {
        Objects.requireNonNull(dataset);
        if (!dataset.supportsTransactions()) {
            throw new IllegalArgumentException("The provided dataset does not support transactions.");
        }
        synchronized (this.dataset) {
            if (this.dataset.isInTransaction()) {
                throw new IllegalStateException("Cannot replace dataset when it is in transaction.");
            }
            this.dataset = dataset;
        }
    }
}
