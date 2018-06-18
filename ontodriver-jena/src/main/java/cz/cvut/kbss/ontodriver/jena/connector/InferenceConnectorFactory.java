package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;

import java.util.HashMap;
import java.util.Map;

public class InferenceConnectorFactory extends SharedConnectorBasedConnectorFactory {

    private final Map<String, String> reasonerConfig;

    public InferenceConnectorFactory(DriverConfiguration configuration, Map<String, String> reasonerConfig) {
        super(configuration);
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
}
