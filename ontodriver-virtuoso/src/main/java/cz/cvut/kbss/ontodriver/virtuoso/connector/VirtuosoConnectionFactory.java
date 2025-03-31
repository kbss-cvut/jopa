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
package cz.cvut.kbss.ontodriver.virtuoso.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactory;
import cz.cvut.kbss.ontodriver.rdf4j.connector.RepoConnection;
import cz.cvut.kbss.ontodriver.rdf4j.connector.StorageConnection;
import cz.cvut.kbss.ontodriver.virtuoso.VirtuosoDriverException;
import org.eclipse.rdf4j.common.transaction.IsolationLevel;
import org.eclipse.rdf4j.repository.Repository;

public class VirtuosoConnectionFactory implements ConnectionFactory {

    private boolean open = true;

    private final VirtuosoStorageConnector storageConnector;
    private final IsolationLevel txIsolationLevel;

    public VirtuosoConnectionFactory(DriverConfiguration config,
                                     IsolationLevel txIsolationLevel) throws VirtuosoDriverException {
        this.txIsolationLevel = txIsolationLevel;
        this.storageConnector = new VirtuosoStorageConnector(config);
        storageConnector.initializeRepository();
    }


    @Override
    public RepoConnection createStorageConnection() {
        if (!open) {
            throw new IllegalStateException("The factory is closed!");
        }
        return new StorageConnection(storageConnector, txIsolationLevel);
    }

    @Override
    public synchronized void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        storageConnector.close();
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void setRepository(Repository repository) {
        throw new UnsupportedOperationException("Not supported by Virtuoso driver.");
    }
}
