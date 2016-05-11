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
package cz.cvut.kbss.ontodriver.owlapi.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;

import java.util.Map;

public abstract class ConnectorFactory {

    private static ConnectorFactory instance = new BasicConnectorFactory();

    public static synchronized ConnectorFactory getInstance() {
        if (!instance.isOpen()) {
            instance = new BasicConnectorFactory();
        }
        return instance;
    }

    public abstract Connector getConnector(OntologyStorageProperties storageProperties,
                                           Configuration configuration) throws OwlapiDriverException;

    public abstract void close() throws OntoDriverException;

    public abstract boolean isOpen();
}
