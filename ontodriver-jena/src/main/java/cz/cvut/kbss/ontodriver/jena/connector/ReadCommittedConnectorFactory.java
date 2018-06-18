package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;

/**
 * Creates connectors implementing the {@link cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties#READ_COMMITTED}
 * transactional strategy.
 */
public class ReadCommittedConnectorFactory extends SharedConnectorBasedConnectorFactory {

    public ReadCommittedConnectorFactory(DriverConfiguration configuration) {
        super(configuration);
    }

    @Override
    public StorageConnector createConnector() {
        ensureOpen();
        return new ChangeTrackingStorageConnector(centralConnector);
    }
}
