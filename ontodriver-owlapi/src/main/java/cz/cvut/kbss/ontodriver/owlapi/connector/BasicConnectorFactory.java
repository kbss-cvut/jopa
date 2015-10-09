package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.OwlapiDriverException;

import java.util.Map;

public class BasicConnectorFactory extends ConnectorFactory {

    // TODO Implement pooling connectors!!!!!

    private boolean open;

    BasicConnectorFactory() {
        this.open = true;
    }

    @Override
    public Connector getConnector(OntologyStorageProperties storageProperties, Map<String, String> properties)
            throws OwlapiDriverException {
        if (!open) {
            throw new IllegalStateException("The factory is closed.");
        }
        return new BasicStorageConnector(storageProperties, properties);
    }

    @Override
    public boolean isOpen() {
        return false;
    }

    @Override
    public void close() throws OntoDriverException {
        this.open = false;
    }
}
