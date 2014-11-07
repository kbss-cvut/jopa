package cz.cvut.kbss.ontodriver.sesame.connector;

import java.util.Map;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

final class ConnectorFactoryImpl extends ConnectorFactory {

	private boolean open;

	private volatile Connector centralConnector;

	protected ConnectorFactoryImpl() {
		this.open = true;
	}

	public Connector createStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws SesameDriverException {
		if (!open) {
			throw new IllegalStateException("The factory is closed!");
		}
		if (centralConnector == null) {
			synchronized (this) {
				if (centralConnector == null) {
					initCentralConnector(storageProperties, properties);
				}
			}
		}
		return new PoolingStorageConnector(centralConnector);
	}

	private void initCentralConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws SesameDriverException {
		this.centralConnector = new StorageConnector(storageProperties, properties);
	}

	public synchronized void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		if (centralConnector != null) {
			centralConnector.close();
			this.centralConnector = null;
		}
		this.open = false;
	}

	public boolean isOpen() {
		return open;
	}
}
