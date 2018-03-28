package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.*;
import org.apache.jena.system.Txn;
import org.apache.jena.update.UpdateAction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
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
        verifyOpen();
        transaction.begin();
        storage.begin(ReadWrite.WRITE);
    }

    @Override
    public synchronized void commit() throws JenaDriverException {
        verifyOpen();
        transaction.verifyActive();
        transaction.commit();
        storage.writeChanges();
        storage.commit();
        transaction.afterCommit();
    }

    @Override
    public void rollback() {
        verifyOpen();
        transaction.rollback();
        storage.rollback();
        transaction.afterRollback();
    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value) {
        verifyOpen();
        return Txn.calculateRead(storage.getDataset(), () -> {
            final StmtIterator it = storage.getDefaultGraph().listStatements(subject, property, value);
            return it.toList();
        });
    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value, String context) {
        verifyOpen();
        return Txn.calculateRead(storage.getDataset(), () -> {
            final StmtIterator it = storage.getNamedGraph(context).listStatements(subject, property, value);
            return it.toList();
        });
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value) {
        verifyOpen();
        return Txn.calculateRead(storage.getDataset(),
                () -> storage.getDefaultGraph().contains(subject, property, value));
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value, String context) {
        verifyOpen();
        return Txn.calculateRead(storage.getDataset(),
                () -> storage.getNamedGraph(context).contains(subject, property, value));
    }

    @Override
    public List<String> getContexts() {
        verifyOpen();
        final Iterator<String> it = Txn.calculateRead(storage.getDataset(), () -> storage.getDataset().listNames());
        final List<String> contexts = new ArrayList<>();
        it.forEachRemaining(contexts::add);
        return contexts;
    }

    @Override
    public void add(List<Statement> statements) {
        verifyOpen();
        transaction.verifyActive();
        storage.add(statements);
    }

    @Override
    public void add(List<Statement> statements, String context) {
        verifyOpen();
        transaction.verifyActive();
        storage.add(statements, context);
    }

    @Override
    public void remove(List<Statement> statements) {
        verifyOpen();
        transaction.verifyActive();
        storage.remove(statements);
    }

    @Override
    public void remove(List<Statement> statements, String context) {
        verifyOpen();
        transaction.verifyActive();
        storage.remove(statements, context);
    }

    @Override
    public void remove(Resource subject, Property property, RDFNode object) {
        verifyOpen();
        transaction.verifyActive();
        storage.remove(storage.getDefaultGraph().listStatements(subject, property, object));
    }

    @Override
    public void remove(Resource subject, Property property, RDFNode object, String context) {
        verifyOpen();
        transaction.verifyActive();
        storage.remove(storage.getNamedGraph(context).listStatements(subject, property, object), context);

    }

    @Override
    public ResultSet executeSelectQuery(String query) throws JenaDriverException {
        try {
            QueryExecution exec = QueryExecutionFactory.create(query, storage.getDataset());
            return exec.execSelect();
        } catch (RuntimeException e) {
            throw new JenaDriverException("Execution of query " + query + " failed.", e);
        }
    }

    @Override
    public boolean executeAskQuery(String query) throws JenaDriverException {
        try {
            QueryExecution exec = QueryExecutionFactory.create(query, storage.getDataset());
            return exec.execAsk();
        } catch (RuntimeException e) {
            throw new JenaDriverException("Execution of query " + query + " failed.", e);
        }
    }

    @Override
    public void executeUpdate(String query) throws JenaDriverException {
        try {
            UpdateAction.parseExecute(query, storage.getDataset());
        } catch (RuntimeException e) {
            throw new JenaDriverException("Execution of update " + query + " failed.", e);
        }
    }

    @Override
    public synchronized void close() {
        storage.close();
        super.close();
    }
}
