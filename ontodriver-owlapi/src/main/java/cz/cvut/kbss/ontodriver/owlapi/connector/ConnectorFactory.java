package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;

import java.util.Map;

public abstract class ConnectorFactory {

    private static ConnectorFactory instance = new BasicConnectorFactory();

    public static synchronized ConnectorFactory getInstance() {
        if (!instance.isOpen()) {
            instance = new BasicConnectorFactory();
        }
        return instance;
    }

    public abstract Connector getConnector(OntologyStorageProperties storageProperties,
                                                     Map<String, String> properties) throws OwlapiDriverException;

    public abstract void close() throws OntoDriverException;

    public abstract boolean isOpen();
}
