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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.ConfigParam;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.config.OwlapiConfigParam;
import cz.cvut.kbss.ontodriver.owlapi.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.list.OwlapiLists;

import java.util.*;

class OwlapiDriver implements Closeable, ConnectionListener {

    private static final List<ConfigurationParameter> CONFIGS = Arrays
            .asList(ConfigParam.AUTO_COMMIT, ConfigParam.MODULE_EXTRACTION_SIGNATURE, ConfigParam.ONTOLOGY_LANGUAGE,
                    ConfigParam.REASONER_FACTORY_CLASS,
                    OwlapiConfigParam.IRI_MAPPING_DELIMITER, OwlapiConfigParam.MAPPING_FILE_LOCATION,
                    OwlapiConfigParam.WRITE_ON_COMMIT);

    private final Configuration configuration;
    private boolean open = true;

    private ConnectorFactory connectorFactory;
    private final Set<OwlapiConnection> openConnections = new HashSet<>();

    OwlapiDriver(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        this.configuration = new Configuration(storageProperties);
        configuration.addConfiguration(properties, CONFIGS);
        this.connectorFactory = ConnectorFactory.createFactory();
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        for (OwlapiConnection c : openConnections) {
            try {
                c.removeListener(this);
                c.close();
            } catch (OntoDriverException e) {
                throw e;
            } catch (Exception e) {
                throw new OwlapiDriverException(e);
            }
        }
        connectorFactory.close();
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    Connection acquireConnection() throws OntoDriverException {
        assert open;
        final OwlapiAdapter adapter = new OwlapiAdapter(connectorFactory.getConnector(configuration), configuration);
        final OwlapiConnection c = new OwlapiConnection(adapter);
        c.setTypes(new OwlapiTypes(adapter, c::ensureOpen, c::commitIfAuto));
        c.setProperties(new OwlapiProperties(adapter, c::ensureOpen, c::commitIfAuto));
        c.setLists(new OwlapiLists(adapter, c::ensureOpen, c::commitIfAuto));
        openConnections.add(c);
        c.addListener(this);
        return c;
    }

    @Override
    public void connectionClosed(Connection connection) {
        openConnections.remove(connection);
    }
}
