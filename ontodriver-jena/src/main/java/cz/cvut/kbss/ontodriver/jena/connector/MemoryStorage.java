package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.DatasetFactory;

/**
 * Storage accessor using an in-memory dataset.
 */
class MemoryStorage extends Storage {

    @Override
    void initialize() {
        this.dataset = DatasetFactory.createTxnMem();
    }
}
