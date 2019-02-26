/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.eclipse.rdf4j.repository.Repository;

final class ConnectorFactoryImpl implements ConnectorFactory {

    private boolean open;

    private volatile StorageConnector centralConnector;

    ConnectorFactoryImpl() {
        this.open = true;
    }

    @Override
    public Connector createStorageConnector(DriverConfiguration configuration) throws SesameDriverException {
        ensureOpen();
        ensureConnected(configuration);
        return new PoolingStorageConnector(centralConnector);
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The factory is closed!");
        }
    }

    private void ensureConnected(DriverConfiguration configuration) throws SesameDriverException {
        if (centralConnector == null) {
            synchronized (this) {
                if (centralConnector == null) {
                    initCentralConnector(configuration);
                }
            }
        }
    }

    private void initCentralConnector(DriverConfiguration configuration) throws SesameDriverException {
        this.centralConnector = new StorageConnector(configuration);
    }

    @Override
    public void setRepository(Repository repository, DriverConfiguration configuration) throws SesameDriverException {
        ensureOpen();
        ensureConnected(configuration);
        centralConnector.setRepository(repository);
    }

    @Override
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

    @Override
    public boolean isOpen() {
        return open;
    }
}
