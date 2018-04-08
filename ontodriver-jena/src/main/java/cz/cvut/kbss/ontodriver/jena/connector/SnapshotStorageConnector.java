package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.jena.config.JenaConfigParam;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.query.AbstractResultSet;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * This connector implements the {@link cz.cvut.kbss.ontodriver.jena.config.JenaOntoDriverProperties#SNAPSHOT}-based transactional strategy.
 * <p>
 * It is also used when inference is required from the driver.
 */
public class SnapshotStorageConnector extends AbstractStorageConnector {

    private final AbstractStorageConnector centralConnector;

    private LocalModel transactionalChanges;

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
        ensureState();
        transaction.commit();
        try {
            centralConnector.begin();
            applyRemovals();
            applyAdditions();
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

    @Override
    public void rollback() {
        ensureState();
        transaction.rollback();
        cleanup();
        transaction.afterRollback();
    }

    private void cleanup() {
        this.storage = null;
        this.transactionalChanges = null;
    }

    private void ensureState() {
        ensureOpen();
        transaction.verifyActive();
    }

    @Override
    public List<Statement> find(Resource subject, Property property, RDFNode value) {
        ensureState();
        return storage.getDefaultGraph().listStatements(subject, property, value).toList();
    }

    @Override
    public List<Statement> find(Resource subject, Property property, RDFNode value, String context) {
        ensureState();
        return storage.getNamedGraph(context).listStatements(subject, property, value).toList();
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value) {
        ensureState();
        return storage.getDefaultGraph().contains(subject, property, value);
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value, String context) {
        ensureState();
        return storage.getNamedGraph(context).contains(subject, property, value);
    }

    @Override
    public List<String> getContexts() {
        ensureState();
        final Iterator<String> contexts = storage.getDataset().listNames();
        final List<String> result = new ArrayList<>();
        contexts.forEachRemaining(result::add);
        return result;
    }

    @Override
    public void add(List<Statement> statements) {
        ensureState();
        storage.add(statements);
        transactionalChanges.addStatements(statements);
    }

    @Override
    public void add(List<Statement> statements, String context) {
        ensureState();
        storage.add(statements, context);
        transactionalChanges.addStatements(statements, context);
    }

    @Override
    public void remove(List<Statement> statements) {
        ensureState();
        storage.remove(statements);
        transactionalChanges.removeStatements(statements);
    }

    @Override
    public void remove(List<Statement> statements, String context) {
        ensureState();
        storage.remove(statements, context);
        transactionalChanges.removeStatements(statements, context);
    }

    @Override
    public void remove(Resource subject, Property property, RDFNode object) {
        ensureState();
        final List<Statement> toRemove = find(subject, property, object);
        remove(toRemove);
    }

    @Override
    public void remove(Resource subject, Property property, RDFNode object, String context) {
        ensureState();
        final List<Statement> toRemove = find(subject, property, object, context);
        remove(toRemove, context);
    }

    @Override
    public AbstractResultSet executeSelectQuery(Query query) throws JenaDriverException {
        return null;
    }

    @Override
    public AbstractResultSet executeAskQuery(Query query) throws JenaDriverException {
        return null;
    }

    @Override
    public void executeUpdate(String query) throws JenaDriverException {

    }
}
