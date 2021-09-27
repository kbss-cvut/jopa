/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;

import java.util.Objects;

/**
 * Storage accessor using an in-memory dataset.
 */
class MemoryStorage extends LocalStorage {

    MemoryStorage(DriverConfiguration configuration) {
        super(configuration);
        this.dataset = DatasetFactory.createTxnMem();
    }

    @Override
    public void setDataset(Dataset dataset) {
        Objects.requireNonNull(dataset);
        if (!dataset.supportsTransactions()) {
            throw new IllegalArgumentException("The provided dataset does not support transactions.");
        }
        synchronized (this) {
            if (this.dataset.isInTransaction()) {
                throw new IllegalStateException("Cannot replace dataset when it is in transaction.");
            }
            this.dataset = dataset;
        }
    }
}
