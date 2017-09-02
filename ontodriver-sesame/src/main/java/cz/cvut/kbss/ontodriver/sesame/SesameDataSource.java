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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

public class SesameDataSource implements DataSource {

    private SesameDriver driver;
    private volatile boolean open = true;
    private boolean connected;

    private OntologyStorageProperties storageProperties;
    private Map<String, String> properties;

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

    @Override
    public synchronized Connection getConnection() throws OntoDriverException {
        if (!open) {
            throw new IllegalStateException("The data source is closed.");
        }
        if (!connected) {
            connect();
        }
        return driver.acquireConnection();
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) throws OntoDriverException {
        this.storageProperties = Objects.requireNonNull(storageProperties);
    }

    @Override
    public void setProperties(Map<String, String> properties) throws OntoDriverException {
        this.properties = Objects.requireNonNull(properties);
    }

    private void connect() {
        if (storageProperties == null) {
            throw new IllegalStateException("Cannot initialize OntoDriver without storageProperties configuration.");
        }
        if (properties == null) {
            this.properties = Collections.emptyMap();
        }
        this.driver = new SesameDriver(storageProperties, properties);
        this.connected = true;
    }
}
