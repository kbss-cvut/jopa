package cz.cvut.kbss.ontodriver.sesame;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;

class SesameDriver implements Closeable, ConnectionListener {

	private final OntologyStorageProperties storageProperties;
	private final Map<String, String> properties;
	private boolean open;

	private final Set<Connection> openedConnections;

	SesameDriver(OntologyStorageProperties storageProperties, Map<String, String> properties) {
		assert storageProperties != null;
		assert properties != null;

		this.storageProperties = storageProperties;
		this.properties = properties;
		this.openedConnections = new HashSet<>();
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		try {
			for (Connection c : openedConnections) {
				c.close();
			}
			ConnectorFactory.close();
		} catch (Exception e) {
			if (e instanceof OntoDriverException || e instanceof SesameDriverException) {
				throw (OntoDriverException) e;
			} else {
				throw new SesameDriverException(e);
			}
		} finally {
			this.open = false;
		}
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	Connection acquireConnection() {
		assert open;
		final Connection c = new SesameConnection(new SesameAdapter(
				ConnectorFactory.createStorageConnector(storageProperties, properties)));
		openedConnections.add(c);
		return c;
	}

	@Override
	public void connectionClosed(Connection connection) {
		if (connection == null) {
			return;
		}
		openedConnections.remove(connection);
	}
}
