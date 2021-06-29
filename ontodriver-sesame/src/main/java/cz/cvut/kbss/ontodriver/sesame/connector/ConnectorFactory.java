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
package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import org.eclipse.rdf4j.repository.Repository;

public interface ConnectorFactory {

    /**
     * Creates a storage connector.
     * <p>
     * It is possible that the underlying implementation will return some sort of proxy for a single storage connector.
     *
     * @param configuration Connector configuration
     * @return New storage connector
     * @throws SesameDriverException When storage access error occurs
     */
    Connector createStorageConnector(DriverConfiguration configuration) throws SesameDriverException;

    /**
     * Closes this factory
     *
     * @throws SesameDriverException When storage access error occurs
     */
    void close() throws OntoDriverException;

    /**
     * Whether this factory is open.
     *
     * @return Factory status
     */
    boolean isOpen();

    /**
     * Sets the underlying repository.
     * <p>
     * Note that this functionality is supported only for in-memory stores.
     *
     * @param repository    The new repository
     * @param configuration Original driver configuration. May be necessary if the factory has not been initialized, yet
     * @throws SesameDriverException In case setting the repository fails
     */
    void setRepository(Repository repository, DriverConfiguration configuration) throws SesameDriverException;

    static ConnectorFactory getInstance() {
        return new ConnectorFactoryImpl();
    }
}
