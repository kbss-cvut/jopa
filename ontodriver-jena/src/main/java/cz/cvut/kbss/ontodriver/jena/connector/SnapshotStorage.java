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
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.system.Txn;

import java.util.Iterator;

/**
 * Represents a snapshot of the main dataset.
 *
 * Can be used to apply transactional changes and be thrown away in case the transaction is rolled back.
 */
class SnapshotStorage extends LocalStorage {

    SnapshotStorage(DriverConfiguration configuration) {
        super(configuration);
        this.dataset = DatasetFactory.create();
    }

    void addCentralData(Dataset central) {
        Txn.executeRead(central, () -> {
            final Iterator<String> it = central.listNames();
            while (it.hasNext()) {
                final String name = it.next();
                dataset.addNamedModel(name, central.getNamedModel(name));
            }
            dataset.setDefaultModel(central.getDefaultModel());
        });
    }
}
