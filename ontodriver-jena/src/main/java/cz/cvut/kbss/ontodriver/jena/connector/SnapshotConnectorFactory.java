package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.Dataset;

/**
 * Creates connectors implementing the {@link cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties#SNAPSHOT}
 * transactional strategy.
 */
public class SnapshotConnectorFactory extends ConnectorFactory {

    private final SharedStorageConnector centralConnector;

    public SnapshotConnectorFactory(DriverConfiguration configuration) {
        this.centralConnector = new SharedStorageConnector(configuration);
    }

    @Override
    public StorageConnector createConnector() {
        ensureOpen();
        return new SnapshotStorageConnector(centralConnector);
    }

    @Override
    public synchronized void reloadStorage() {
        ensureOpen();
        centralConnector.reloadStorage();
    }

    @Override
    public void setDataset(Dataset dataset) {
        ensureOpen();
        centralConnector.setDataset(dataset);
    }

    @Override
    public synchronized void close() throws JenaDriverException {
        super.close();
        centralConnector.close();
    }
}
