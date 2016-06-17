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
package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;

public class BasicConnectorFactory implements ConnectorFactory {

    private boolean open;

    private AbstractConnector connector;

    BasicConnectorFactory() {
        this.open = true;
    }

    @Override
    public synchronized AbstractConnector getConnector(OntologyStorageProperties storageProperties,
                                                       Configuration configuration) throws OwlapiDriverException {
        if (!open) {
            throw new IllegalStateException("The factory is closed.");
        }
        if (connector == null) {
            initConnector(storageProperties, configuration);
        }
        return connector;
    }

    private void initConnector(OntologyStorageProperties storageProperties, Configuration configuration)
            throws OwlapiDriverException {
        this.connector = new BasicStorageConnector(storageProperties, configuration);
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public synchronized void close() throws OntoDriverException {
        if (connector != null) {
            connector.close();
        }
        this.open = false;
    }
}
