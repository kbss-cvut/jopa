package cz.cvut.kbss.ontodriver.sesame.connector;

import java.util.Map;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

public final class ConnectorFactory {

	private static boolean open = true;

	private static volatile Connector centralConnector;

	private ConnectorFactory() {
		throw new AssertionError();
	}

	public static Connector createStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws SesameDriverException {
		if (centralConnector == null) {
			synchronized (centralConnector) {
				if (centralConnector == null) {
					initCentralConnector(storageProperties, properties);
				}
			}
		}
		return new PoolingStorageConnector(centralConnector);
	}

	private static void initCentralConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws SesameDriverException {
		centralConnector = new StorageConnector(storageProperties, properties);
	}

	public static synchronized void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		if (centralConnector != null) {
			centralConnector.close();
		}
		open = false;
	}

}
