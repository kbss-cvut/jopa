package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.Collection;

/**
 * This connector does not support inference, it wraps a regular {@link StorageConnector} and calls its regular methods
 * instead of performing any inference, e.g. {@link StorageConnector#contains(Resource, Property, RDFNode)} for {@link #containsWithInference(Resource, Property, RDFNode)}.
 * Thus, inference-based methods give the same results as regular connector methods.
 */
class DummyInferredStorageConnector implements InferredStorageConnector {

    private final StorageConnector connector;

    DummyInferredStorageConnector(StorageConnector connector) {
        this.connector = connector;
    }

    @Override
    public Collection<Statement> findWithInference(Resource subject, Property property, RDFNode value) {
        return connector.find(subject, property, value);
    }

    @Override
    public Collection<Statement> findWithInference(Resource subject, Property property, RDFNode value, String context) {
        return connector.find(subject, property, value, context);
    }

    @Override
    public boolean containsWithInference(Resource subject, Property property, RDFNode value) {
        return connector.contains(subject, property, value);
    }

    @Override
    public boolean containsWithInference(Resource subject, Property property, RDFNode value, String context) {
        return connector.contains(subject, property, value, context);
    }

    @Override
    public boolean isConsistent() {
        return true;
    }

    @Override
    public boolean isConsistent(String context) {
        return true;
    }
}
