/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame.connector;

import java.util.Map;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
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
