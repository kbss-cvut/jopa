package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;

abstract class ConnectorFactory implements Closeable {

    private boolean open = true;

    @Override
    public synchronized void close() throws JenaDriverException {
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    void verifyOpen() {
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
}
