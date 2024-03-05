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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.eclipse.rdf4j.repository.Repository;

public final class ConnectionFactoryImpl implements ConnectionFactory {

    private boolean open;

    private final StorageConnector connector;
    private final boolean isGraphDB;

    public ConnectionFactoryImpl(StorageConnector connector) {
        this(connector, false);
    }

    public ConnectionFactoryImpl(StorageConnector connector, boolean isGraphDB) {
        this.open = true;
        this.connector = connector;
        this.isGraphDB = isGraphDB;
    }

    @Override
    public RepoConnection createStorageConnection() {
        ensureOpen();
        if (isGraphDB) {
            return new GraphDBStorageConnection(connector);
        } else {
            return new StorageConnection(connector);
        }
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The factory is closed!");
        }
    }

    @Override
    public void setRepository(Repository repository) {
        ensureOpen();
        connector.setRepository(repository);
    }

    @Override
    public synchronized void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        if (connector != null) {
            connector.close();
        }
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}
