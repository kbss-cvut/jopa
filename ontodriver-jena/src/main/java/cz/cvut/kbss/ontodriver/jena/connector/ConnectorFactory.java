/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.Dataset;

public abstract class ConnectorFactory implements Closeable {

    private volatile boolean open = true;

    @Override
    public synchronized void close() throws JenaDriverException {
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("Factory is closed.");
        }
    }

    /**
     * Creates a storage connector.
     *
     * @return storage connector
     */
    public abstract StorageConnector createConnector();

    /**
     * Creates an inference-supporting storage connector.
     * <p>
     * The {@code connector} parameter is required because both connector need to be kept in sync so that non-inferred
     * data are consistent across both connectors.
     *
     * @param connector Existing storage connector
     * @return New inference-supporting storage connector
     */
    public InferredStorageConnector createInferredConnector(StorageConnector connector) {
        return new DummyInferredStorageConnector(connector);
    }

    /**
     * Reloads data from storage if it is a file-based one.
     * <p>
     * Does nothing for other types of storage.
     */
    public abstract void reloadStorage();

    /**
     * Sets dataset on the underlying connector.
     * <p>
     * Not that this operation is supported only for in-memory storage.
     *
     * @param dataset Dataset to set
     */
    public abstract void setDataset(Dataset dataset);
}
