/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;

/**
 * Common superclass for the storage connectors.
 * <p>
 * Declares the basic interface and stores storage info.
 */
abstract class AbstractConnector implements Closeable, Connector {

    final DriverConfiguration configuration;

    private volatile boolean open;

    AbstractConnector(DriverConfiguration configuration) throws OwlapiDriverException {
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
