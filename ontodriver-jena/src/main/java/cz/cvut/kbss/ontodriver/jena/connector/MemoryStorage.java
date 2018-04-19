package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import org.apache.jena.query.DatasetFactory;

/**
 * Storage accessor using an in-memory dataset.
 */
class MemoryStorage extends Storage {

    MemoryStorage(Configuration configuration) {
        super(configuration);
    }

    @Override
    void initialize() {
        this.dataset = DatasetFactory.createTxnMem();
    }
}
