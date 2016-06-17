/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

public interface ConnectorFactory {

    /**
     * Creates a storage connector.
     * <p>
     * It is possible that the underlying implementation will return some sort of proxy for a single storage connector.
     *
     * @param storageProperties Storage connection properties
     * @param configuration     Connector configuration
     * @return New storage connector
     * @throws SesameDriverException
     */
    Connector createStorageConnector(OntologyStorageProperties storageProperties,
                                     Configuration configuration) throws SesameDriverException;

    /**
     * Closes this factory
     *
     * @throws OntoDriverException
     */
    void close() throws OntoDriverException;

    /**
     * Whether this factory is open.
     *
     * @return Factory status
     */
    boolean isOpen();

    static ConnectorFactory getInstance() {
        return new ConnectorFactoryImpl();
    }
}
