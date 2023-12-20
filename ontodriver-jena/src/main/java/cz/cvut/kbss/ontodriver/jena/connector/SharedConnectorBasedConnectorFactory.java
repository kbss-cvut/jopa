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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.Dataset;

/**
 * Abstracts code common to connector factories which use one central connector and create transactional connectors on demand.
 */
abstract class SharedConnectorBasedConnectorFactory extends ConnectorFactory {

    final SharedStorageConnector centralConnector;

    SharedConnectorBasedConnectorFactory(DriverConfiguration configuration) {
        this.centralConnector = new SharedStorageConnector(configuration);
    }

    @Override
    public synchronized void reloadStorage() {
        ensureOpen();
        centralConnector.reloadStorage();
    }

    @Override
    public void setDataset(Dataset dataset) {
        ensureOpen();
        centralConnector.setDataset(dataset);
    }

    @Override
    public synchronized void close() throws JenaDriverException {
        super.close();
        centralConnector.close();
    }
}
