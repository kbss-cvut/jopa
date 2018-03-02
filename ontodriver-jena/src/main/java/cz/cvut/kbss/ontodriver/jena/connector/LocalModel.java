package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdf.model.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * Tracks transactional changes.
 */
class LocalModel {

    private final Dataset added;
    private final Dataset removed;

    /**
     * Whether default graph should be treated as union of all graphs.
     */
    private final boolean defaultAsUnion;

    enum Containment {
        ADDED, REMOVED, UNKNOWN
    }

    LocalModel(boolean defaultAsUnion) {
        this.added = DatasetFactory.create();
        this.removed = DatasetFactory.create();
        this.defaultAsUnion = defaultAsUnion;
    }

    List<Statement> enhanceStatements(Collection<Statement> statements, Resource subject, Property property,
                                      RDFNode value) {
        return enhanceStatements(statements, subject, property, value, addedDefault(), removedDefault());
    }

    private Model addedDefault() {
        return defaultAsUnion ? added.getUnionModel().union(added.getDefaultModel()) : added.getDefaultModel();
    }

    private Model removedDefault() {
        return defaultAsUnion ? removed.getUnionModel().union(removed.getDefaultModel()) : removed.getDefaultModel();
    }

    List<Statement> enhanceStatements(Collection<Statement> statements, Resource subject, Property property,
                                      RDFNode value, String context) {
        return enhanceStatements(statements, subject, property, value, added.getNamedModel(context),
                removed.getNamedModel(context));
    }

    private List<Statement> enhanceStatements(Collection<Statement> toEnhance, Resource subject,
                                              Property property, RDFNode value, Model addedModel,
                                              Model removedModel) {
        final List<Statement> statements = new ArrayList<>(toEnhance);
        statements.addAll(addedModel.listStatements(subject, property, value).toList());
        statements.removeAll(removedModel.listStatements(subject, property, value).toList());
        return statements;
    }

    Containment contains(Resource subject, Property property, RDFNode value) {
        if (removedDefault().contains(subject, property, value)) {
            return Containment.REMOVED;
        } else {
            return addedDefault().contains(subject, property, value) ? Containment.ADDED : Containment.UNKNOWN;
        }
    }

    Containment contains(Resource subject, Property property, RDFNode value, String context) {
        if (removed.getNamedModel(context).contains(subject, property, value)) {
            return Containment.REMOVED;
        } else {
            return added.getNamedModel(context).contains(subject, property, value) ? Containment.ADDED :
                   Containment.UNKNOWN;
        }
    }

    void addStatements(List<Statement> statements) {
        added.getDefaultModel().add(statements);
        removed.getDefaultModel().remove(statements);
        if (defaultAsUnion) {
            removed.listNames().forEachRemaining(n -> removed.getNamedModel(n).remove(statements));
        }
    }

    void addStatements(List<Statement> statements, String context) {
        added.getNamedModel(context).add(statements);
        removed.getNamedModel(context).remove(statements);
    }

    void removeStatements(List<Statement> statements) {
        removed.getDefaultModel().add(statements);
        added.getDefaultModel().remove(statements);
        if (defaultAsUnion) {
            added.listNames().forEachRemaining(n -> added.getNamedModel(n).remove(statements));
        }
    }

    void removeStatements(List<Statement> statements, String context) {
        removed.getNamedModel(context).add(statements);
        added.getNamedModel(context).remove(statements);
    }

    Dataset getAdded() {
        return added;
    }

    Dataset getRemoved() {
        return removed;
    }

    List<String> getContexts() {
        final Iterator<String> it = added.listNames();
        final List<String> contexts = new ArrayList<>();
        it.forEachRemaining(contexts::add);
        return contexts;
    }
}
