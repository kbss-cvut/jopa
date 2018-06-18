package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.Dataset;

/**
 * Abstracts code common to connector factories which use one central connector and create transactional connectors on demand.
 */
abstract class SharedConnectorBasedConnectorFactory extends ConnectorFactory {

    final SharedStorageConnector centralConnector;

    SharedConnectorBasedConnectorFactory(DriverConfiguration configuration) {
        this.centralConnector = new SharedStorageConnector(configuration);
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
