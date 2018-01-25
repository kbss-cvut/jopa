package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.*;

import java.util.Collection;
import java.util.List;

/**
 * This connector tracks transactional changes and writes them on commit to the {@link SharedStorageConnector}.
 */
public class ChangeTrackingStorageConnector extends AbstractStorageConnector {

    private final AbstractStorageConnector centralConnector;

    private LocalModel localModel;

    ChangeTrackingStorageConnector(AbstractStorageConnector centralConnector) {
        this.centralConnector = centralConnector;
    }

    @Override
    public void begin() {
        transaction.begin();
        this.localModel = new LocalModel();
    }

    @Override
    public void commit() throws JenaDriverException {
        transaction.commit();
        try {
            centralConnector.begin();
            mergeRemovedStatements();
            mergeAddedStatements();
            centralConnector.commit();
            transaction.afterCommit();
        } catch (JenaDriverException e) {
            transaction.rollback();
            centralConnector.rollback();
            transaction.afterRollback();
            throw e;
        } finally {
            this.localModel = null;
        }
    }

    private void mergeRemovedStatements() {
        final Dataset removed = localModel.getRemoved();
        centralConnector.remove(removed.getDefaultModel().listStatements().toList());
        removed.listNames().forEachRemaining(context -> {
            final Model model = removed.getNamedModel(context);
            centralConnector.remove(model.listStatements().toList(), context);
        });
    }

    private void mergeAddedStatements() {
        final Dataset added = localModel.getAdded();
        centralConnector.add(added.getDefaultModel().listStatements().toList());
        added.listNames().forEachRemaining(context -> {
            final Model model = added.getNamedModel(context);
            centralConnector.add(model.listStatements().toList(), context);
        });
    }

    @Override
    public void rollback() {
        transaction.rollback();
        this.localModel = null;
        transaction.afterRollback();
    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value) {
        transaction.verifyActive();
        final Collection<Statement> existing = centralConnector.find(subject, property, value);
        return localModel.enhanceStatements(existing, subject, property, value);
    }

    @Override
    public Collection<Statement> find(Resource subject, Property property, RDFNode value, String context) {
        transaction.verifyActive();
        final Collection<Statement> existing = centralConnector.find(subject, property, value, context);
        return localModel.enhanceStatements(existing, subject, property, value, context);
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value) {
        transaction.verifyActive();
        final LocalModel.Containment localStatus = localModel.contains(subject, property, value);
        return localStatus == LocalModel.Containment.ADDED ||
                localStatus == LocalModel.Containment.UNKNOWN && centralConnector.contains(subject, property, value);
    }

    @Override
    public boolean contains(Resource subject, Property property, RDFNode value, String context) {
        transaction.verifyActive();
        final LocalModel.Containment localStatus = localModel.contains(subject, property, value, context);
        return localStatus == LocalModel.Containment.ADDED ||
                localStatus == LocalModel.Containment.UNKNOWN &&
                        centralConnector.contains(subject, property, value, context);
    }

    @Override
    public void add(List<Statement> statements) {
        transaction.verifyActive();
        localModel.addStatements(statements);
    }

    @Override
    public void add(List<Statement> statements, String context) {
        transaction.verifyActive();
        localModel.addStatements(statements, context);
    }

    @Override
    public void remove(List<Statement> statements) {
        transaction.verifyActive();
        localModel.removeStatements(statements);
    }

    @Override
    public void remove(List<Statement> statements, String context) {
        transaction.verifyActive();
        localModel.removeStatements(statements, context);
    }
}
