package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.DataSource;

import java.util.Map;

/**
 * This DataSource implementations is invalid because it does not have a public no-arg constructor.
 * <p/>
 * Created by ledvima1 on 12.12.14.
 */
public class InvalidDataSource implements DataSource {

    public InvalidDataSource(Object arg) {
        // Don't do anything, just prevent existence of no-arg constructor
    }

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
