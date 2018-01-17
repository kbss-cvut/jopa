package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

import java.util.Map;

public class JenaDataSource implements DataSource {

    @Override
    public Connection getConnection() throws OntoDriverException {
        return null;
    }

    @Override
    public void setStorageProperties(OntologyStorageProperties storageProperties) throws OntoDriverException {

    }

    @Override
    public void setProperties(Map<String, String> properties) throws OntoDriverException {

    }

    @Override
    public void close() throws OntoDriverException {

    }

    @Override
    public boolean isOpen() {
        return false;
    }
}
