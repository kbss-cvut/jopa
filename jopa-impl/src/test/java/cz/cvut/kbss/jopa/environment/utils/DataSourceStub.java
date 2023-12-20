/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;

import java.util.Map;

public class DataSourceStub implements DataSource {

    private boolean open = true;
    private Connection connection;

    public void setConnection(Connection connection) {
        this.connection = connection;
    }

    @Override
    public Connection getConnection() {
        return connection;
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) {

    }

    @Override
    public void setProperties(Map<String, String> properties) {

    }

    @Override
    public void close() {
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}
