package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;

abstract class AbstractStorageConnector implements StorageConnector {

    final Configuration configuration;

    private volatile boolean open;

    Storage storage;

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

    abstract void initialize();

    <T> T unwrap(Class<T> cls) {
        if (cls.isAssignableFrom(Storage.class)) {
            return cls.cast(storage);
        }
        throw new UnsupportedOperationException("Unwrapping type " + cls + "not supported.");
    }
}
