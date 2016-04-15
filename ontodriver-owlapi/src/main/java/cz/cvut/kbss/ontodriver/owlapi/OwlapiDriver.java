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
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.owlapi.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver.owlapi.exception.OwlapiDriverException;
import cz.cvut.kbss.ontodriver.owlapi.list.OwlapiLists;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

class OwlapiDriver implements Closeable, ConnectionListener {

    private final OntologyStorageProperties storageProperties;
    private final Map<String, String> properties;
    private boolean open = true;

    private final Set<OwlapiConnection> openConnections = new HashSet<>();

    OwlapiDriver(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        this.storageProperties = storageProperties;
        this.properties = properties;
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
            } catch (Exception e) {
                if (e instanceof OntoDriverException) {
                    throw (OntoDriverException) e;
                } else {
                    throw new OwlapiDriverException(e);
                }
            }
        }
        ConnectorFactory.getInstance().close();
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    Connection acquireConnection() throws OntoDriverException {
        assert open;
        final OwlapiAdapter adapter = new OwlapiAdapter(
                ConnectorFactory.getInstance().getConnector(storageProperties, properties), properties);
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
