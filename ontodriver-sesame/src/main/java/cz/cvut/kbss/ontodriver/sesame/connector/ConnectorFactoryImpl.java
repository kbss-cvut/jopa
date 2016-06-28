/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

public final class ConnectorFactoryImpl implements ConnectorFactory {

    private boolean open;

    private volatile Connector centralConnector;

    ConnectorFactoryImpl() {
        this.open = true;
    }

    public Connector createStorageConnector(Configuration configuration) throws SesameDriverException {
        if (!open) {
            throw new IllegalStateException("The factory is closed!");
        }
        if (centralConnector == null) {
            synchronized (this) {
                if (centralConnector == null) {
                    initCentralConnector(configuration);
                }
            }
        }
        return new PoolingStorageConnector(centralConnector);
    }

    private void initCentralConnector(Configuration configuration) throws SesameDriverException {
        this.centralConnector = new StorageConnector(configuration);
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
