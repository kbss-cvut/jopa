package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.ReloadableDataSource;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

public class JenaDataSource implements ReloadableDataSource {

    private volatile boolean open = true;

    private OntologyStorageProperties storageProperties;
    private Map<String, String> properties;

    private JenaDriver driver;

    @Override
    public synchronized Connection getConnection() {
        ensureOpen();
        if (driver == null) {
            connect();
        }
        return driver.acquireConnection();
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The data source is closed.");
        }
    }

    private void connect() {
        if (storageProperties == null) {
            throw new IllegalStateException("Data source cannot connect without ontology storage properties.");
        }
        this.driver = new JenaDriver(storageProperties, properties != null ? properties : Collections.emptyMap());
    }

    @Override
    public synchronized void setStorageProperties(OntologyStorageProperties storageProperties) {
        this.storageProperties = Objects.requireNonNull(storageProperties);
    }

    @Override
    public synchronized void setProperties(Map<String, String> properties) {
        this.properties = Objects.requireNonNull(properties);
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        try {
            if (driver != null) {
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

    @Override
    public void reload() throws JenaDriverException {
        ensureOpen();
        driver.reloadStorage();
    }
}
