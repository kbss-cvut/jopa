package cz.cvut.kbss.ontodriver.virtuoso;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

/**
 * {@link DataSource} implementation representing the Virtuoso OntoDriver.
 */
public class VirtuosoDataSource implements DataSource {

    private VirtuosoDriver driver;
    private volatile boolean open = true;
    private boolean connected;

    private OntologyStorageProperties storageProperties;
    private Map<String, String> properties;

    @Override
    public synchronized Connection getConnection() throws VirtuosoDriverException {
        ensureOpen();
        ensureConnected();
        return driver.acquireConnection();
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The data source is closed.");
        }
    }

    private void ensureConnected() throws VirtuosoDriverException {
        if (connected) {
            return;
        }
        if (storageProperties == null) {
            throw new IllegalStateException("Cannot initialize OntoDriver without storageProperties configuration.");
        }
        if (properties == null) {
            this.properties = Collections.emptyMap();
        }
        this.driver = new VirtuosoDriver(storageProperties, properties);
        this.connected = true;
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) {
        ensureOpen();
        this.storageProperties = Objects.requireNonNull(storageProperties);
    }

    @Override
    public void setProperties(Map<String, String> properties) {
        ensureOpen();
        this.properties = Objects.requireNonNull(properties);
    }

    @Override
    public synchronized void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        try {
            if (connected) {
                driver.close();
            }
        } finally {
            this.open = false;
        }
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}
