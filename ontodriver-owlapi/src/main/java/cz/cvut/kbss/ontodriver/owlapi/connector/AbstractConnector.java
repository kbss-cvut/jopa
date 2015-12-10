package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;

import java.util.Map;

/**
 * Common superclass for the storage connectors.
 * <p>
 * Declares the basic interface and stores storage info.
 *
 * @author ledvima1
 */
abstract class AbstractConnector implements Closeable, Connector {

    protected OntologyStorageProperties storageProperties;
    protected Map<String, String> properties;

    private volatile boolean open;

    public AbstractConnector(OntologyStorageProperties storageProperties, Map<String, String> properties) throws
            OwlapiDriverException {
        assert storageProperties != null;
        assert properties != null;

        this.storageProperties = storageProperties;
        this.properties = properties;
        initializeConnector();
        this.open = true;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void close() throws OntoDriverException {
        this.open = false;
    }

    protected void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The connector is closed.");
        }
    }

    /**
     * Initializes the connector.
     */
    protected abstract void initializeConnector() throws OwlapiDriverException;
}
