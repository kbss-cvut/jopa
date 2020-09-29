/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.jena.connector;

import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdf.model.*;

import java.util.*;

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

    Collection<Statement> enhanceStatements(Collection<Statement> statements, Resource subject, Property property,
                                            RDFNode value, Collection<String> contexts) {
        if (contexts.isEmpty()) {
            return enhanceStatements(statements, subject, property, value, addedDefault(), removedDefault());
        } else {
            Collection<Statement> enhanced = statements;
            for (String ctx : contexts) {
                enhanced = enhanceStatements(statements, subject, property, value, added.getNamedModel(ctx),
                        removed.getNamedModel(ctx));
            }
            return enhanced;
        }
    }

    private Model addedDefault() {
        return defaultAsUnion ? added.getUnionModel().union(added.getDefaultModel()) : added.getDefaultModel();
    }

    private Model removedDefault() {
        return defaultAsUnion ? removed.getUnionModel().union(removed.getDefaultModel()) : removed.getDefaultModel();
    }

    private static Collection<Statement> enhanceStatements(Collection<Statement> toEnhance, Resource subject,
                                                           Property property, RDFNode value, Model addedModel,
                                                           Model removedModel) {
        final Set<Statement> statements = new HashSet<>(toEnhance);
        statements.addAll(addedModel.listStatements(subject, property, value).toList());
        statements.removeAll(removedModel.listStatements(subject, property, value).toList());
        return statements;
    }

    Containment contains(Resource subject, Property property, RDFNode value, String context) {
        final Model removedModel = context != null ? removed.getNamedModel(context) : removedDefault();
        final Model addedModel = context != null ? added.getNamedModel(context) : addedDefault();
        if (removedModel.contains(subject, property, value)) {
            return Containment.REMOVED;
        } else {
            return addedModel.contains(subject, property, value) ? Containment.ADDED :
                   Containment.UNKNOWN;
        }
    }

    void addStatements(List<Statement> statements, String context) {
        if (context != null) {
            added.getNamedModel(context).add(statements);
            removed.getNamedModel(context).remove(statements);
        } else {
            added.getDefaultModel().add(statements);
            removed.getDefaultModel().remove(statements);
            if (defaultAsUnion) {
                removed.listNames().forEachRemaining(n -> removed.getNamedModel(n).remove(statements));
            }
        }
    }

    void removeStatements(List<Statement> statements, String context) {
        if (context != null) {
            removed.getNamedModel(context).add(statements);
            added.getNamedModel(context).remove(statements);
        } else {
            removed.getDefaultModel().add(statements);
            added.getDefaultModel().remove(statements);
            if (defaultAsUnion) {
                added.listNames().forEachRemaining(n -> added.getNamedModel(n).remove(statements));
            }
        }
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
