/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.repository.Repository;

public final class ConnectorFactoryImpl implements ConnectorFactory {

    private boolean open;

    private StorageConnector centralConnector;

    public ConnectorFactoryImpl(StorageConnector connector) {
        this.open = true;
        this.centralConnector = connector;
    }

    @Override
    public Connector createStorageConnector() {
        ensureOpen();
        return new PoolingStorageConnector(centralConnector);
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The factory is closed!");
        }
    }

    @Override
    public void setRepository(Repository repository) {
        ensureOpen();
        centralConnector.setRepository(repository);
    }

    @Override
    public synchronized void close() throws Rdf4jDriverException {
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
