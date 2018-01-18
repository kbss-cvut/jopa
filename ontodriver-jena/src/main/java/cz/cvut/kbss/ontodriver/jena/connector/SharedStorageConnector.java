package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.*;

import java.util.Collection;

/**
 * Main storage connector using the {@link cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties#READ_COMMITTED} connector strategy.
 * <p>
 * Adding statements to it actually adds them to the repository.
 */
public class SharedStorageConnector extends AbstractStorageConnector {

    SharedStorageConnector(Configuration configuration) {
        super(configuration);
    }

    @Override
    void initialize() {
        this.storage = Storage.create(configuration);
    }

    @Override
    public void begin() {
        // Do nothing. The shared connector is not transactional
    }

    @Override
    public void commit() {
        // Do nothing. The shared connector is not transactional
    }

    @Override
    public void rollback() {
        // Do nothing. The shared connector is not transactional
    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value) {
        storage.getDataset().begin(ReadWrite.READ);
        try {
            final StmtIterator it = storage.getDataset().getDefaultModel().listStatements(subject, property, value);
            return it.toList();
        } finally {
            storage.getDataset().commit();
        }
    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value, String context) {
        storage.getDataset().begin(ReadWrite.READ);
        try {
            final Model targetGraph = storage.getDataset().getNamedModel(context);
            return targetGraph.listStatements(subject, property, value).toList();
        } finally {
            storage.getDataset().commit();
        }
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value) {
        storage.getDataset().begin(ReadWrite.READ);
        try {
            return storage.getDataset().getDefaultModel().contains(subject, property, value);
        } finally {
            storage.getDataset().commit();
        }
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value, String context) {
        storage.getDataset().begin(ReadWrite.READ);
        try {
            final Model targetGraph = storage.getDataset().getNamedModel(context);
            return targetGraph.contains(subject, property, value);
        } finally {
            storage.getDataset().commit();
        }
    }

    @Override
    public void add(Collection<Statement> statements) {

    }

    @Override
    public void remove(Collection<Statement> statements) {

    }

    @Override
    public synchronized void close() {
        storage.close();
        super.close();
    }
}
