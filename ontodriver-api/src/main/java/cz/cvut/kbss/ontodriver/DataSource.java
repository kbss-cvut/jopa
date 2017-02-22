/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.Map;

/**
 * Represents an ontology data source.
 * <p>
 * This could be either a single ontology or a storage with multiple named graphs or ontology modules.
 * <p>
 * The implementations are required to have a public no-arg constructor. Connection settings are passed through the
 * setter methods.
 */
public interface DataSource extends Closeable {

    /**
     * Requests a connection to the underlying data source.
     *
     * @return A {@code Connection} to the data source
     * @throws OntoDriverException If an ontology access error occurs
     */
    Connection getConnection() throws OntoDriverException;

    /**
     * Sets storage properties for this data source.
     * <p>
     * Note that if the data source is already connected to the underlying storage, the outcome of this method is not
     * specified and is likely to have no effect.
     *
     * @param storageProperties The properties to use
     * @throws OntoDriverException If an ontology access error occurs
     */
    void setStorageProperties(OntologyStorageProperties storageProperties) throws OntoDriverException;

    /**
     * Sets additional configuration properties on the data source.
     *
     * @param properties Map of properties
     * @throws OntoDriverException If an ontology access error occurs
     */
    void setProperties(Map<String, String> properties) throws OntoDriverException;
}
