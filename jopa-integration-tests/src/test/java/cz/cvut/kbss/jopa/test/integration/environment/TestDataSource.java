package cz.cvut.kbss.jopa.test.integration.environment;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.Map;

public class TestDataSource implements DataSource {

    private Connection connection;
    private boolean open = true;

    public void setConnection(Connection connection) {
        this.connection = connection;
    }

    @Override
    public Connection getConnection() throws OntoDriverException {
        return connection;
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) throws OntoDriverException {
        // Do nothing
    }

    @Override
    public void setProperties(Map<String, String> properties) throws OntoDriverException {
        // Do nothing
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
