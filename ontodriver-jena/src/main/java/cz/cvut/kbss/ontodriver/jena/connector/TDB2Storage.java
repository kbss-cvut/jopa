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
package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import org.apache.jena.dboe.base.file.Location;
import org.apache.jena.tdb2.TDB2Factory;

class TDB2Storage extends LocalStorage {

    TDB2Storage(DriverConfiguration configuration) {
        super(configuration);
        String targetDir = configuration.getStorageProperties().getPhysicalURI().toString();
        if (targetDir.startsWith(FILE_PREFIX)) {
            targetDir = configuration.getStorageProperties().getPhysicalURI().getSchemeSpecificPart();
        }
        Location location = Location.create(targetDir);
        this.dataset = TDB2Factory.connectDataset(location);
    }

    // Changes are written automatically on commit by TDB2
}
