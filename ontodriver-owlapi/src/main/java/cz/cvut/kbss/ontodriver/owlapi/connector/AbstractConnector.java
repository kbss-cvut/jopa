/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;

/**
 * Common superclass for the storage connectors.
 * <p>
 * Declares the basic interface and stores storage info.
 */
abstract class AbstractConnector implements Closeable, Connector {

    final Configuration configuration;

    private volatile boolean open;

    AbstractConnector(Configuration configuration) throws OwlapiDriverException {
        assert configuration != null;

        this.configuration = configuration;
        initializeConnector();
        this.open = true;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void close() throws OntoDriverException {
        this.open = false;
    }

    protected void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The connector is closed.");
        }
    }

    /**
     * Reloads ontology from the underlying storage.
     */
    void reloadData() throws OwlapiDriverException {
        // Do nothing by default
    }

    /**
     * Initializes the connector.
     *
     * @throws OwlapiDriverException When storage access error occurs
     */
    protected abstract void initializeConnector() throws OwlapiDriverException;
}
