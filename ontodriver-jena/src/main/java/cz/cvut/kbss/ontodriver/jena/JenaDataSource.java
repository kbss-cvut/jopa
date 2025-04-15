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
package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.ReloadableDataSource;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.Dataset;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

public class JenaDataSource implements ReloadableDataSource {

    private volatile boolean open = true;

    private OntologyStorageProperties storageProperties;
    private Map<String, String> properties;

    private JenaDriver driver;

    @Override
    public synchronized Connection getConnection() {
        ensureOpen();
        if (driver == null) {
            connect();
        }
        return driver.acquireConnection();
    }

    private void ensureOpen() {
        if (!open) {
            throw new IllegalStateException("The data source is closed.");
        }
    }

    private void connect() {
        if (storageProperties == null) {
            throw new IllegalStateException("Data source cannot connect without ontology storage properties.");
        }
        this.driver = new JenaDriver(storageProperties, properties != null ? properties : Collections.emptyMap());
    }

    @Override
    public synchronized void setStorageProperties(OntologyStorageProperties storageProperties) {
        this.storageProperties = Objects.requireNonNull(storageProperties);
    }

    @Override
    public synchronized void setProperties(Map<String, String> properties) {
        this.properties = Objects.requireNonNull(properties);
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        try {
            if (driver != null) {
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
    public void reload() throws JenaDriverException {
        ensureOpen();
        driver.reloadStorage();
    }

    /**
     * Sets dataset in the underlying storage.
     * <p>
     * Not that this operation is supported only for in-memory storage.
     *
     * @param dataset Dataset to set
     * @throws JenaDriverException If setting new dataset fails
     */
    public void setDataset(Dataset dataset) throws JenaDriverException {
        ensureOpen();
        driver.setDataset(dataset);
    }
}
