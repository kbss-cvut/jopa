package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;

/**
 * Creates connectors implementing the {@link cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties#READ_COMMITTED}
 * transactional strategy.
 */
public class ReadCommittedConnectorFactory extends ConnectorFactory {

    private final SharedStorageConnector centralConnector;

    public ReadCommittedConnectorFactory(DriverConfiguration configuration) {
        this.centralConnector = new SharedStorageConnector(configuration);
    }

    @Override
    public StorageConnector createConnector() {
        ensureOpen();
        return new ChangeTrackingStorageConnector(centralConnector);
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
