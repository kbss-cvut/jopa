package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;

/**
 * Creates connectors implementing the {@link cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties#SNAPSHOT}
 * transactional strategy.
 */
public class SnapshotConnectorFactory extends SharedConnectorBasedConnectorFactory {

    public SnapshotConnectorFactory(DriverConfiguration configuration) {
        super(configuration);
    }

    @Override
    public StorageConnector createConnector() {
        ensureOpen();
        return new SnapshotStorageConnector(centralConnector);
    }
}
