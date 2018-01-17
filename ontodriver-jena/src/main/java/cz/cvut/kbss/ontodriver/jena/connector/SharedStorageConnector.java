package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;

/**
 * Main storage connector using the shared connector strategy.
 *
 * Adding statements to it actually adds them to the repository.
 */
public class SharedStorageConnector implements StorageConnector {

    private volatile boolean open;

    @Override
    public void begin() {

    }

    @Override
    public void commit() {

    }

    @Override
    public void rollback() {

    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value) {
        return null;
    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value, String context) {
        return null;
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value) {
        return false;
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value, String context) {
        return false;
    }

    @Override
    public void add(Collection<Statement> statements) {

    }

    @Override
    public void remove(Collection<Statement> statements) {

    }

    @Override
    public void close() throws OntoDriverException {

    }

    @Override
    public boolean isOpen() {
        return open;
    }
}
