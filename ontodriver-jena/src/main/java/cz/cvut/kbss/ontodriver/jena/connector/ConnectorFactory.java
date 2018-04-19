package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;

public abstract class ConnectorFactory implements Closeable {

    private volatile boolean open = true;

    @Override
    public synchronized void close() throws JenaDriverException {
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("Factory is closed.");
        }
    }

    /**
     * Creates a storage connector.
     *
     * @return storage connector
     */
    public abstract StorageConnector createConnector();

    /**
     * Creates an inference-supporting storage connector.
     * <p>
     * The {@code connector} parameter is required because both connector need to be kept in sync so that
     * non-inferred data are consistent across both connectors.
     *
     * @param connector Existing storage connector
     * @return New inference-supporting storage connector
     */
    public InferredStorageConnector createInferredConnector(StorageConnector connector) {
        return new DummyInferredStorageConnector(connector);
    }

    /**
     * Reloads data from storage if it is a file-based one.
     * <p>
     * Does nothing for other types of storage.
     */
    public abstract void reloadStorage();
}
