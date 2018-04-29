package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.util.Transaction;
import org.apache.jena.query.Dataset;

/**
 * Base implementation of the {@link StorageConnector} interface.
 */
abstract class AbstractStorageConnector implements StorageConnector {

    final DriverConfiguration configuration;

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

    AbstractStorageConnector(DriverConfiguration configuration) {
        this.configuration = configuration;
        initialize();
        this.open = true;
    }

    Storage getStorage() {
        return storage;
    }

    @Override
    public synchronized void close() {
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    void ensureOpen() {
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

    @Override
    public <T> T unwrap(Class<T> cls) {
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        } else if (cls.isAssignableFrom(Dataset.class)) {
            return cls.cast(storage.dataset);
        }
        throw new UnsupportedOperationException("Unwrapping type " + cls + "not supported.");
    }
}
