package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;

/**
 * Creates connectors implementing the {@link cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties#READ_COMMITTED}
 * transactional strategy.
 */
public class ReadCommittedConnectorFactory extends ConnectorFactory {

    private final SharedStorageConnector centralConnector;

    public ReadCommittedConnectorFactory(Configuration configuration) {
        this.centralConnector = new SharedStorageConnector(configuration);
    }

    @Override
    public synchronized void close() throws JenaDriverException {
        super.close();
        centralConnector.close();
    }

    @Override
    public synchronized StorageConnector createConnector() {
        ensureOpen();
        return new ChangeTrackingStorageConnector(centralConnector);
    }
}
