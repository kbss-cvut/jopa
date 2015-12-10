package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
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

    private DataSource initDataSource(OntologyStorageProperties storageProperties, Map<String, String> properties) {
        final Class<?> dataSourceCls;
        try {
            dataSourceCls = Class.forName(storageProperties.getDriver());
            DataSource ds = (DataSource) dataSourceCls.newInstance();
            ds.setStorageProperties(storageProperties);
            if (properties != null) {
                ds.setProperties(properties);
            }
            return ds;
        } catch (ClassNotFoundException e) {
            throw new DataSourceCreationException(
                    "Unable to find OntoDriver data source class " + storageProperties.getDriver(), e);
        } catch (InstantiationException | IllegalAccessException | OntoDriverException e) {
            throw new DataSourceCreationException(
                    "Unable to create instance of OntoDriver data source " + storageProperties.getDriver(), e);
        }
    }

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
            throw new StorageAccessException(e);
        } finally {
            this.open = false;
        }
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}
