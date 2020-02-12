/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.utils.Wrapper;
import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Connection;

public interface StorageAccessor extends Closeable, Wrapper {

    /**
     * Acquires a connection to the underlying ontology driver.
     *
     * @return Connection to the storage
     * @throws StorageAccessException If an error occurs when acquiring connection from the underlying driver
     */
    Connection acquireConnection();
}
