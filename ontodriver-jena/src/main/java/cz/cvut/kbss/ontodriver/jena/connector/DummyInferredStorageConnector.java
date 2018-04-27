package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.query.AbstractResultSet;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;

/**
 * This connector does not support inference, it wraps a regular {@link StorageConnector} and calls its regular methods
 * instead of performing any inference, e.g. {@link StorageConnector#contains(Resource, Property, RDFNode, String)} for
 * {@link #containsWithInference(Resource, Property, RDFNode, String)}.
 * <p>
 * Thus, inference-based methods give the same results as regular connector methods.
 */
class DummyInferredStorageConnector implements InferredStorageConnector {

    private final StorageConnector connector;

    DummyInferredStorageConnector(StorageConnector connector) {
        this.connector = connector;
    }

    @Override
    public Collection<Statement> findWithInference(Resource subject, Property property, RDFNode value, String context) {
        return connector.find(subject, property, value, context);
    }

    @Override
    public boolean containsWithInference(Resource subject, Property property, RDFNode value, String context) {
        return connector.contains(subject, property, value, context);
    }

    @Override
    public boolean isConsistent(String context) {
        return true;
    }

    @Override
    public AbstractResultSet executeSelectQuery(Query query,
                                                cz.cvut.kbss.ontodriver.Statement.StatementOntology target) throws
                                                                                                            JenaDriverException {
        return connector.executeSelectQuery(query, target);
    }

    @Override
    public AbstractResultSet executeAskQuery(Query query,
                                             cz.cvut.kbss.ontodriver.Statement.StatementOntology target) throws
                                                                                                         JenaDriverException {
        return connector.executeAskQuery(query, target);
    }

    @Override
    public void executeUpdate(String query, cz.cvut.kbss.ontodriver.Statement.StatementOntology target) throws
                                                                                                        JenaDriverException {
        connector.executeUpdate(query, target);
    }
}
