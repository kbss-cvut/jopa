package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;
import java.util.List;

/**
 * This connector tracks transactional changes and writes them on commit to the {@link SharedStorageConnector}.
 */
public class ChangeTrackingStorageConnector extends AbstractStorageConnector {

    private final AbstractStorageConnector centralConnector;

    ChangeTrackingStorageConnector(AbstractStorageConnector centralConnector) {
        this.centralConnector = centralConnector;
    }

    @Override
    public void begin() {

    }

    @Override
    public void commit() throws JenaDriverException {

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
    public void add(List<Statement> statements) {

    }

    @Override
    public void add(List<Statement> statements, String context) {

    }

    @Override
    public void remove(List<Statement> statements) {

    }

    @Override
    public void remove(List<Statement> statements, String context) {

    }
}
