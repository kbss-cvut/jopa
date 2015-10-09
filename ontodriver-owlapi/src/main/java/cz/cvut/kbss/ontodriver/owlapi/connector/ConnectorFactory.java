package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

import java.util.Map;

public class ConnectorFactory {

    private static ConnectorFactory instance;

    private final OntologyStorageProperties storageProperties;
    private final Map<String, String> properties;

    private Connector connector;

    protected ConnectorFactory(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        this.storageProperties = storageProperties;
        this.properties = properties;
    }

    public static synchronized ConnectorFactory getInstance(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        if (instance == null) {
            instance = new ConnectorFactory(storageProperties, properties);
        }
        return instance;
    }

    public synchronized Connector getConnector() throws OntoDriverException {
        if (connector == null) {
            this.connector = new BasicStorageConnector(storageProperties, properties);
        }
        return connector;
    }

    public synchronized void close() throws OntoDriverException {
        if (connector != null) {
            connector.close();
            this.connector = null;
        }
    }
}
