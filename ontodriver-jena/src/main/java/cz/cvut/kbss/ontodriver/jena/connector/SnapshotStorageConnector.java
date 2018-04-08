package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.Statement.StatementOntology;
import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.query.AbstractResultSet;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.update.UpdateAction;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * This connector implements the {@link cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties#SNAPSHOT}-based transactional strategy.
 * <p>
 * It is also used when inference is required from the driver.
 */
public class SnapshotStorageConnector extends SharedStorageConnector {

    private final AbstractStorageConnector centralConnector;

    private LocalModel transactionalChanges;
    private List<String> transactionalUpdates;

    SnapshotStorageConnector(AbstractStorageConnector centralConnector) {
        super(centralConnector.configuration);
        this.centralConnector = centralConnector;
    }

    @Override
    public void begin() {
        ensureOpen();
        if (transaction.isActive()) {
            throw new IllegalStateException("Transaction is already active.");
        }
        transaction.begin();
        snapshotCentralDataset();
        this.transactionalUpdates = new ArrayList<>();
        this.transactionalChanges = new LocalModel(configuration.is(JenaConfigParam.TREAT_DEFAULT_GRAPH_AS_UNION));
    }

    private void snapshotCentralDataset() {
        final TransactionalSnapshotStorage s = new TransactionalSnapshotStorage(configuration);
        s.initialize();
        s.addCentralData(centralConnector.getStorage().getDataset());
        this.storage = s;
    }

    @Override
    public void commit() throws JenaDriverException {
        ensureTransactionalState();
        transaction.commit();
        try {
            centralConnector.begin();
            applyRemovals();
            applyAdditions();
            applyTransactionUpdateQueries();
            centralConnector.commit();
        } finally {
            cleanup();
            transaction.afterCommit();
        }

    }

    private void applyRemovals() {
        final Dataset removed = transactionalChanges.getRemoved();
        centralConnector.remove(removed.getDefaultModel().listStatements().toList());
        removed.listNames()
               .forEachRemaining(n -> centralConnector.remove(removed.getNamedModel(n).listStatements().toList(), n));
    }

    private void applyAdditions() {
        final Dataset added = transactionalChanges.getAdded();
        centralConnector.add(added.getDefaultModel().listStatements().toList());
        added.listNames()
             .forEachRemaining(n -> centralConnector.add(added.getNamedModel(n).listStatements().toList(), n));
    }

    private void applyTransactionUpdateQueries() throws JenaDriverException {
        for (String query : transactionalUpdates) {
            centralConnector.executeUpdate(query, StatementOntology.CENTRAL);
        }
    }

    @Override
    public void rollback() {
        ensureTransactionalState();
        transaction.rollback();
        cleanup();
        transaction.afterRollback();
    }

    private void cleanup() {
        this.storage = null;
        this.transactionalChanges = null;
        this.transactionalUpdates = null;
    }


    @Override
    public List<Statement> find(Resource subject, Property property, RDFNode value) {
        ensureTransactionalState();
        return storage.getDefaultGraph().listStatements(subject, property, value).toList();
    }

    @Override
    public List<Statement> find(Resource subject, Property property, RDFNode value, String context) {
        ensureTransactionalState();
        return storage.getNamedGraph(context).listStatements(subject, property, value).toList();
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value) {
        ensureTransactionalState();
        return storage.getDefaultGraph().contains(subject, property, value);
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value, String context) {
        ensureTransactionalState();
        return storage.getNamedGraph(context).contains(subject, property, value);
    }

    @Override
    public List<String> getContexts() {
        ensureTransactionalState();
        final Iterator<String> contexts = storage.getDataset().listNames();
        final List<String> result = new ArrayList<>();
        contexts.forEachRemaining(result::add);
        return result;
    }

    @Override
    public void add(List<Statement> statements) {
        ensureTransactionalState();
        storage.add(statements);
        transactionalChanges.addStatements(statements);
    }

    @Override
    public void add(List<Statement> statements, String context) {
        ensureTransactionalState();
        storage.add(statements, context);
        transactionalChanges.addStatements(statements, context);
    }

    @Override
    public void remove(List<Statement> statements) {
        ensureTransactionalState();
        storage.remove(statements);
        transactionalChanges.removeStatements(statements);
    }

    @Override
    public void remove(List<Statement> statements, String context) {
        ensureTransactionalState();
        storage.remove(statements, context);
        transactionalChanges.removeStatements(statements, context);
    }

    @Override
    public void remove(Resource subject, Property property, RDFNode object) {
        ensureTransactionalState();
        final List<Statement> toRemove = find(subject, property, object);
        remove(toRemove);
    }

    @Override
    public void remove(Resource subject, Property property, RDFNode object, String context) {
        ensureTransactionalState();
        final List<Statement> toRemove = find(subject, property, object, context);
        remove(toRemove, context);
    }

    @Override
    public AbstractResultSet executeSelectQuery(Query query, StatementOntology target) throws JenaDriverException {
        ensureOpen();
        if (target == StatementOntology.TRANSACTIONAL) {
            transaction.verifyActive();
            return super.executeSelectQuery(query, target);
        } else {
            return centralConnector.executeSelectQuery(query, target);
        }
    }

    @Override
    public AbstractResultSet executeAskQuery(Query query, StatementOntology target) throws JenaDriverException {
        ensureOpen();
        if (target == StatementOntology.TRANSACTIONAL) {
            transaction.verifyActive();
            return super.executeAskQuery(query, target);
        } else {
            return centralConnector.executeAskQuery(query, target);
        }
    }

    @Override
    public void executeUpdate(String query, StatementOntology target) throws JenaDriverException {
        ensureOpen();
        if (target == StatementOntology.TRANSACTIONAL) {
            transaction.verifyActive();
            try {
                UpdateAction.parseExecute(query, storage.getDataset());
                transactionalUpdates.add(query);
            } catch (RuntimeException e) {
                throw new JenaDriverException("Execution of update " + query + " failed.", e);
            }
        } else {
            centralConnector.executeUpdate(query, target);
        }
    }
}
