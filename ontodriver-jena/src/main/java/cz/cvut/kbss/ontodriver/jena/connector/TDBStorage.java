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
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.tdb.TDB;
import org.apache.jena.tdb.TDBFactory;

class TDBStorage extends LocalStorage {

    private static final String FILE_PREFIX = "file:";

    TDBStorage(DriverConfiguration configuration) {
        super(configuration);
        String location = configuration.getStorageProperties().getPhysicalURI().toString();
        if (location.startsWith(FILE_PREFIX)) {
            location = configuration.getStorageProperties().getPhysicalURI().getSchemeSpecificPart();
        }
        this.dataset = TDBFactory.createDataset(location);
    }

    @Override
    public void writeChanges() throws JenaDriverException {
        try {
            TDB.sync(dataset);
        } catch (RuntimeException e) {
            throw new JenaDriverException("Unable to synchronize TDB storage with file system.", e);
        }
    }
}
