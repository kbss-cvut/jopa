package cz.cvut.kbss.ontodriver.sesame;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;

public class SesameDataSource implements DataSource {

    private SesameDriver driver;
    private volatile boolean open = true;
    private boolean connected;

    private OntologyStorageProperties storageProperties;
    private Map<String, String> properties;

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

    @Override
    public synchronized Connection getConnection() throws OntoDriverException {
        if (!open) {
            throw new IllegalStateException("The data source is closed.");
        }
        if (!connected) {
            connect();
        }
        return driver.acquireConnection();
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) throws OntoDriverException {
        this.storageProperties = Objects.requireNonNull(storageProperties, ErrorUtils.constructNPXMessage("storageProperties"));
    }

    @Override
    public void setProperties(Map<String, String> properties) throws OntoDriverException {
        this.properties = Objects.requireNonNull(properties, ErrorUtils.constructNPXMessage("properties"));
    }

    private void connect() {
        if (storageProperties == null) {
            throw new IllegalStateException("Cannot initialize OntoDriver without storageProperties configuration.");
        }
        if (properties == null) {
            this.properties = Collections.emptyMap();
        }
        this.driver = new SesameDriver(storageProperties, properties);
        this.connected = true;
    }
}
