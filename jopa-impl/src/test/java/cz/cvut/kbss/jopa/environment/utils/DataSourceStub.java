package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.Map;

public class DataSourceStub implements DataSource {

    private boolean open = true;
    private Connection connection;

    public void setConnection(Connection connection) {
        this.connection = connection;
    }

    @Override
    public Connection getConnection() throws OntoDriverException {
        return connection;
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) throws OntoDriverException {

    }

    @Override
    public void setProperties(Map<String, String> properties) throws OntoDriverException {

    }

    @Override
    public void close() throws OntoDriverException {
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}
