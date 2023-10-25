/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.utils.ReflectionUtils;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;

import java.util.Map;

public class DefaultStorageAccessor implements StorageAccessor {

    private final DataSource dataSource;
    private boolean open;

    public DefaultStorageAccessor(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        this.dataSource = initDataSource(storageProperties, properties);
        this.open = true;
    }

    private static DataSource initDataSource(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        final Class<?> dataSourceCls;
        try {
            dataSourceCls = Class.forName(storageProperties.getDriver());
            DataSource ds = (DataSource) ReflectionUtils.instantiateUsingDefaultConstructor(dataSourceCls);
            ds.setStorageProperties(storageProperties);
            if (properties != null) {
                ds.setProperties(properties);
            }
            return ds;
        } catch (ClassNotFoundException e) {
            throw new DataSourceCreationException(
                    "Unable to find OntoDriver data source class " + storageProperties.getDriver(), e);
        } catch (cz.cvut.kbss.jopa.exception.InstantiationException | OntoDriverException e) {
            throw new DataSourceCreationException(
                    "Unable to create instance of OntoDriver data source " + storageProperties.getDriver(), e);
        }
    }

    @Override
    public Connection acquireConnection() {
        try {
            final Connection conn = dataSource.getConnection();
            conn.setAutoCommit(false);
            return conn;
        } catch (OntoDriverException e) {
            throw new StorageAccessException("Unable to acquire storage connection.", e);
        }
    }

    @Override
    public void close() {
        if (!open) {
            return;
        }
        try {
            dataSource.close();
        } catch (OntoDriverException e) {
            throw new StorageAccessException("Error when closing the data source.", e);
        } finally {
            this.open = false;
        }
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        } else if (cls.isAssignableFrom(dataSource.getClass())) {
            return cls.cast(dataSource);
        }
        throw new OWLPersistenceException("Instance of class " + cls + " not found.");
    }
}
