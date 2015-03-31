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
	private final ConnectorFactory connectorFactory;

	private final Set<SesameConnection> openedConnections;

	SesameDriver(OntologyStorageProperties storageProperties, Map<String, String> properties) {
		assert storageProperties != null;
		assert properties != null;

		this.storageProperties = storageProperties;
		this.properties = properties;
		this.openedConnections = new HashSet<>();
		this.connectorFactory = ConnectorFactory.getInstance();
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		try {
			for (SesameConnection c : openedConnections) {
				c.removeListener(this);
				c.close();
			}
			connectorFactory.close();
		} catch (Exception e) {
			if (e instanceof OntoDriverException) {
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

	Connection acquireConnection() throws SesameDriverException {
		assert open;
		final SesameAdapter adapter = new SesameAdapter(connectorFactory.createStorageConnector(
				storageProperties, properties), properties);
		final SesameConnection c = new SesameConnection(adapter);
		c.setLists(new SesameLists(c, adapter));
		c.setTypes(new SesameTypes(c, adapter));
        c.setProperties(new SesameProperties(c, adapter));
		openedConnections.add(c);
		c.addListener(this);
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
