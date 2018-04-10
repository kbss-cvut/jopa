package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.List;

/**
 * This connector implementation supports proper inference.
 */
class SnapshotStorageConnectorWithInference extends SnapshotStorageConnector implements InferredStorageConnector {

    private SnapshotStorageWithInference storage;

    SnapshotStorageConnectorWithInference(AbstractStorageConnector centralConnector) {
        super(centralConnector);
    }

    @Override
    void snapshotCentralDataset() {
        final SnapshotStorageWithInference s = new SnapshotStorageWithInference(configuration);
        s.initialize();
        s.addCentralData(centralConnector.getStorage().getDataset());
        this.storage = s;
        super.storage = s;  // So that we don't have to override the inherited modification methods
    }

    @Override
    public List<Statement> find(Resource subject, Property property, RDFNode value) {
        ensureTransactionalState();
        return storage.getRawDefaultGraph().listStatements(subject, property, value).toList();
    }

    @Override
    public List<Statement> find(Resource subject, Property property, RDFNode value, String context) {
        ensureTransactionalState();
        return storage.getRawNamedGraph(context).listStatements(subject, property, value).toList();
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value) {
        ensureTransactionalState();
        return storage.getRawDefaultGraph().contains(subject, property, value);
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value, String context) {
        ensureTransactionalState();
        return storage.getRawNamedGraph(context).contains(subject, property, value);
    }

    @Override
    public List<Statement> findWithInference(Resource subject, Property property, RDFNode value) {
        ensureTransactionalState();
        return storage.getDefaultGraph().listStatements(subject, property, value).toList();
    }

    @Override
    public List<Statement> findWithInference(Resource subject, Property property, RDFNode value, String context) {
        ensureTransactionalState();
        return storage.getNamedGraph(context).listStatements(subject, property, value).toList();
    }

    @Override
    public boolean containsWithInference(Resource subject, Property property, RDFNode value) {
        ensureTransactionalState();
        return storage.getDefaultGraph().contains(subject, property, value);
    }

    @Override
    public boolean containsWithInference(Resource subject, Property property, RDFNode value, String context) {
        ensureTransactionalState();
        return storage.getNamedGraph(context).contains(subject, property, value);
    }

    @Override
    public boolean isConsistent() {
        ensureTransactionalState();
        return storage.checkConsistency().isValid();
    }

    @Override
    public boolean isConsistent(String context) {
        ensureTransactionalState();
        return storage.checkConsistency(context).isValid();
    }
}
