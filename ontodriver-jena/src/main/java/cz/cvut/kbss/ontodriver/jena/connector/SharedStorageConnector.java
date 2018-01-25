package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.ReadWrite;
import org.apache.jena.rdf.model.*;
import org.apache.jena.system.Txn;

import java.util.Collection;
import java.util.List;

/**
 * Main storage connector using the {@link cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties#READ_COMMITTED} connector strategy.
 * <p>
 * Adding statements to it actually adds them to the repository.
 * <p>
 * Note on transactions:
 * <p>
 * Starting a transaction on this connector also starts a write transaction on the underlying dataset. Commit then commits
 * the transaction. Therefore, these transactions should be short. Reading can happen in parallel (as per Jena documentation).
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
    public synchronized void begin() {
        transaction.begin();
        storage.getDataset().begin(ReadWrite.WRITE);
    }

    @Override
    public synchronized void commit() throws JenaDriverException {
        transaction.verifyActive();
        transaction.commit();
        storage.writeChanges();
        storage.getDataset().commit();
        transaction.afterCommit();
    }

    @Override
    public void rollback() {
        transaction.verifyActive();
        transaction.rollback();
        storage.getDataset().abort();
        transaction.afterRollback();
    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value) {
        return Txn.calculateRead(storage.getDataset(), () -> {
            final StmtIterator it = storage.getDataset().getDefaultModel().listStatements(subject, property, value);
            return it.toList();
        });
    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value, String context) {
        return Txn.calculateRead(storage.getDataset(), () -> {
            final StmtIterator it = storage.getDataset().getNamedModel(context)
                                           .listStatements(subject, property, value);
            return it.toList();
        });
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value) {
        return Txn.calculateRead(storage.getDataset(),
                () -> storage.getDataset().getDefaultModel().contains(subject, property, value));
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value, String context) {
        return Txn.calculateRead(storage.getDataset(),
                () -> storage.getDataset().getNamedModel(context).contains(subject, property, value));
    }

    @Override
    public void add(List<Statement> statements) {
        transaction.verifyActive();
        storage.getDataset().getDefaultModel().add(statements);
    }

    @Override
    public void add(List<Statement> statements, String context) {
        transaction.verifyActive();
        final Model targetGraph = storage.getDataset().getNamedModel(context);
        targetGraph.add(statements);
    }

    @Override
    public void remove(List<Statement> statements) {
        transaction.verifyActive();
        storage.getDataset().getDefaultModel().remove(statements);
    }

    @Override
    public void remove(List<Statement> statements, String context) {
        transaction.verifyActive();
        final Model targetGraph = storage.getDataset().getNamedModel(context);
        targetGraph.remove(statements);
    }

    @Override
    public synchronized void close() {
        storage.close();
        super.close();
    }
}
