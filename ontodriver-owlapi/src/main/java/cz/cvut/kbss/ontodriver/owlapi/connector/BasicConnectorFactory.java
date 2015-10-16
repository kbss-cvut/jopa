package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;

import java.util.Map;

public class BasicConnectorFactory extends ConnectorFactory {

    private boolean open;

    private AbstractConnector connector;

    BasicConnectorFactory() {
        this.open = true;
    }

    @Override
    public synchronized AbstractConnector getConnector(OntologyStorageProperties storageProperties,
                                                       Map<String, String> properties) throws OwlapiDriverException {
        if (!open) {
            throw new IllegalStateException("The factory is closed.");
        }
        if (connector == null) {
            initConnector(storageProperties, properties);
        }
        return connector;
    }

    private void initConnector(OntologyStorageProperties storageProperties, Map<String, String> properties)
            throws OwlapiDriverException {
        this.connector = new BasicStorageConnector(storageProperties, properties);
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public synchronized void close() throws OntoDriverException {
        if (connector != null) {
            connector.close();
        }
        this.open = false;
    }
}
