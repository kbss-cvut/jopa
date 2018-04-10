package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;

public class InferenceConnectorFactory extends ConnectorFactory {

    private final SharedStorageConnector centralConnector;

    public InferenceConnectorFactory(Configuration configuration) {
        this.centralConnector = new SharedStorageConnector(configuration);
    }

    @Override
    public synchronized StorageConnector createConnector() {
        ensureOpen();
        return new SnapshotStorageConnectorWithInference(centralConnector);
    }

    @Override
    public InferredStorageConnector createInferredConnector(StorageConnector connector) {
        assert connector instanceof SnapshotStorageConnectorWithInference;
        return (InferredStorageConnector) connector;
    }

    @Override
    public synchronized void close() throws JenaDriverException {
        super.close();
        centralConnector.close();
    }
}
