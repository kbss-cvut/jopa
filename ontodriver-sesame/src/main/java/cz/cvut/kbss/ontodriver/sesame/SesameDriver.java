/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.Connection;

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
