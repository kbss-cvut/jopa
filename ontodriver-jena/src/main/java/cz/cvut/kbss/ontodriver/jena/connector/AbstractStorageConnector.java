package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.util.Transaction;

abstract class AbstractStorageConnector implements StorageConnector {

    final Configuration configuration;

    final Transaction transaction = new Transaction();

    private volatile boolean open;

    Storage storage;

    /**
     * Constructs this connector without using any configuration.
     */
    AbstractStorageConnector() {
        this.configuration = null;
        initialize();
        this.open = true;
    }

    AbstractStorageConnector(Configuration configuration) {
        this.configuration = configuration;
        initialize();
        this.open = true;
    }

    @Override
    public synchronized void close() {
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    void verifyOpen() {
        if (!open) {
            throw new IllegalStateException("This connector is closed.");
        }
    }

    /**
     * Initializes this connector.
     * <p>
     * Does nothing by default.
     */
    void initialize() {
        // Do nothing
    }

    <T> T unwrap(Class<T> cls) {
        if (cls.isAssignableFrom(Storage.class)) {
            return cls.cast(storage);
        }
        throw new UnsupportedOperationException("Unwrapping type " + cls + "not supported.");
    }
}
