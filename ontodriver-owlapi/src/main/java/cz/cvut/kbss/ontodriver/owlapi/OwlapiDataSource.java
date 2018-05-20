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
package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.ReloadableDataSource;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

/**
 * Main entry point to this OWLAPI-based OntoDriver.
 *
 * @author ledvima1
 */
public class OwlapiDataSource implements ReloadableDataSource {

    private OntologyStorageProperties storageProperties;
    private Map<String, String> properties;

    private volatile boolean open = true;
    private boolean connected = false;

    private OwlapiDriver driver;

    @Override
    public synchronized Connection getConnection() throws OntoDriverException {
        ensureOpen();
        if (storageProperties == null) {
            throw new IllegalStateException("OntoDriver is not properly initialized. Cannot acquire connection.");
        }
        if (!connected) {
            connect();
        }
        return driver.acquireConnection();
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The OntoDriver is closed.");
        }
    }

    private void connect() {
        this.driver = new OwlapiDriver(storageProperties, properties != null ? properties : Collections.emptyMap());
        this.connected = true;
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) {
        this.storageProperties = Objects.requireNonNull(storageProperties);
    }

    @Override
    public void setProperties(Map<String, String> properties) {
        this.properties = Objects.requireNonNull(properties);
    }

    @Override
    public synchronized void reload() throws OntoDriverException {
        ensureOpen();
        if (connected) {
            driver.reloadData();
        }
    }

    @Override
    public synchronized void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        try {
            if (connected) {
                driver.close();
            }
        } finally {
            this.open = false;
        }
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}
