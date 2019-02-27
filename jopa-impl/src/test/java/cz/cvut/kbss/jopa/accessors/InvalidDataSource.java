/**
 * Copyright (C) 2019 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;

import java.util.Map;

/**
 * This DataSource implementations is invalid because it does not have a public no-arg constructor.
 * <p/>
 * Created by ledvima1 on 12.12.14.
 */
public class InvalidDataSource implements DataSource {

    public InvalidDataSource(Object arg) {
        // Don't do anything, just prevent existence of no-arg constructor
    }

    @Override
    public Connection getConnection() throws OntoDriverException {
        return null;
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) throws OntoDriverException {

    }

    @Override
    public void setProperties(Map<String, String> properties) throws OntoDriverException {

    }

    @Override
    public void close() throws OntoDriverException {

    }

    @Override
    public boolean isOpen() {
        return false;
    }
}
