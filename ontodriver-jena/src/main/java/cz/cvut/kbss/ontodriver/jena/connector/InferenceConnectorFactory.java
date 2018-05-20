package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;

import java.util.HashMap;
import java.util.Map;

public class InferenceConnectorFactory extends ConnectorFactory {

    private final SharedStorageConnector centralConnector;
    private final Map<String, String> reasonerConfig;

    public InferenceConnectorFactory(DriverConfiguration configuration, Map<String, String> reasonerConfig) {
        this.centralConnector = new SharedStorageConnector(configuration);
        this.reasonerConfig = new HashMap<>(reasonerConfig);
    }

    @Override
    public StorageConnector createConnector() {
        ensureOpen();
        return new SnapshotStorageConnectorWithInference(centralConnector, reasonerConfig);
    }

    @Override
    public InferredStorageConnector createInferredConnector(StorageConnector connector) {
        assert connector instanceof SnapshotStorageConnectorWithInference;
        return (InferredStorageConnector) connector;
    }

    @Override
    public synchronized void reloadStorage() {
        ensureOpen();
        centralConnector.reloadStorage();
    }

    @Override
    public synchronized void close() throws JenaDriverException {
        super.close();
        centralConnector.close();
    }
}
