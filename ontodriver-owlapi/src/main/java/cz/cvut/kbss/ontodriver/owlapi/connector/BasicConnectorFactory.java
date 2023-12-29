/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;

public class BasicConnectorFactory implements ConnectorFactory {

    private boolean open;

    private AbstractConnector connector;

    BasicConnectorFactory() {
        this.open = true;
    }

    @Override
    public synchronized AbstractConnector getConnector(DriverConfiguration configuration) throws OwlapiDriverException {
        ensureOpen();
        if (connector == null) {
            initConnector(configuration);
        }
        return connector;
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The factory is closed.");
        }
    }

    private void initConnector(DriverConfiguration configuration) throws OwlapiDriverException {
        this.connector = new BasicStorageConnector(configuration);
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public synchronized void reloadData() throws OwlapiDriverException {
        ensureOpen();
        if (connector == null) {
            return;
        }
        connector.reloadData();
    }

    @Override
    public synchronized void close() throws OntoDriverException {
        if (connector != null) {
            connector.close();
        }
        this.open = false;
    }
}
