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

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.util.Transaction;
import org.apache.jena.query.Dataset;

/**
 * Base implementation of the {@link StorageConnector} interface.
 */
abstract class AbstractStorageConnector implements StorageConnector {

    final DriverConfiguration configuration;

    final Transaction transaction = new Transaction();

    private volatile boolean open;

    Storage storage;

    /**
     * Constructs this connector without using any configuration.
     */
    AbstractStorageConnector() {
        this.configuration = null;
        initialize();
        this.open = true;
    }

    AbstractStorageConnector(DriverConfiguration configuration) {
        this.configuration = configuration;
        initialize();
        this.open = true;
    }

    Storage getStorage() {
        return storage;
    }

    @Override
    public synchronized void close() {
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("This connector is closed.");
        }
    }

    /**
     * Initializes this connector.
     * <p>
     * Does nothing by default.
     */
    void initialize() {
        // Do nothing
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        } else if (cls.isAssignableFrom(Dataset.class)) {
            return cls.cast(storage.getDataset());
        }
        throw new UnsupportedOperationException("Unwrapping type " + cls + " not supported.");
    }
}
