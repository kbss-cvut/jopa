package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.DataSource;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

/**
 * Main entry point to this OWLAPI-based OntoDriver.
 *
 * @author ledvima1
 */
public class OwlapiDataSource implements DataSource {

    private OntologyStorageProperties storageProperties;
    private Map<String, String> properties;

    private volatile boolean open = true;
    private boolean connected = false;

    private OwlapiDriver driver;

    @Override
    public synchronized Connection getConnection() throws OntoDriverException {
        ensureOpen();
        if (storageProperties == null) {
            throw new IllegalStateException("OntoDriver is not properly initialized. Cannot acquire connection.");
        }
        if (!connected) {
            connect();
        }
        return driver.acquireConnection();
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The OntoDriver is closed.");
        }
    }

    private void connect() {
        this.driver = new OwlapiDriver(storageProperties,
                properties != null ? properties : Collections.<String, String>emptyMap());
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) throws OntoDriverException {
        this.storageProperties = Objects.requireNonNull(storageProperties,
                ErrorUtils.constructNPXMessage("storageProperties"));
    }

    @Override
    public void setProperties(Map<String, String> properties) throws OntoDriverException {
        this.properties = Objects.requireNonNull(properties, ErrorUtils.constructNPXMessage("properties"));
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
