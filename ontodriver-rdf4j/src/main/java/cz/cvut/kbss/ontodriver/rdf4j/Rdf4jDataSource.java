/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.repository.Repository;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

/**
 * {@link DataSource} implementation representing the RDF4J OntoDriver.
 */
public class Rdf4jDataSource implements DataSource {

    private Rdf4jDriver driver;
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
        ensureOpen();
        ensureConnected();
        return driver.acquireConnection();
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The data source is closed.");
        }
    }

    private void ensureConnected() throws Rdf4jDriverException {
        if (connected) {
            return;
        }
        if (storageProperties == null) {
            throw new IllegalStateException("Cannot initialize OntoDriver without storageProperties configuration.");
        }
        if (properties == null) {
            this.properties = Collections.emptyMap();
        }
        this.driver = new Rdf4jDriver(storageProperties, properties);
        this.connected = true;
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) {
        ensureOpen();
        this.storageProperties = Objects.requireNonNull(storageProperties);
    }

    @Override
    public void setProperties(Map<String, String> properties) {
        ensureOpen();
        this.properties = Objects.requireNonNull(properties);
    }

    /**
     * Sets the underlying repository.
     * <p>
     * Note that this functionality is supported only for in-memory stores.
     *
     * @param repository The new repository
     * @throws Rdf4jDriverException When setting repository fails
     */
    public synchronized void setRepository(Repository repository) throws Rdf4jDriverException {
        ensureOpen();
        ensureConnected();
        try {
            driver.setRepository(repository);
        } catch (IllegalArgumentException | IllegalStateException e) {
            throw new Rdf4jDriverException(e);
        }
    }
}
