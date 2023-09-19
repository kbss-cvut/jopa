/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.Statement;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Tracks transactional changes.
 */
class LocalModel {

    private final Dataset added;
    private final Dataset removed;
    private final Set<SubjectPredicateContext> removedSubjectPredicateStatements;

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
        this.removedSubjectPredicateStatements = new HashSet<>();
        this.defaultAsUnion = defaultAsUnion;
    }

    Collection<Statement> enhanceStatements(Collection<Statement> statements, Resource subject, Property property,
                                            RDFNode value, Collection<String> contexts) {
        if (contexts.isEmpty()) {
            return enhanceStatements(statements, subject, property, value, addedDefault(), removedDefault(), null);
        } else {
            Collection<Statement> enhanced = statements;
            for (String ctx : contexts) {
                enhanced = enhanceStatements(statements, subject, property, value, added.getNamedModel(ctx),
                        removed.getNamedModel(ctx), ctx);
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

    private Collection<Statement> enhanceStatements(Collection<Statement> toEnhance, Resource subject,
                                                    Property property, RDFNode value, Model addedModel,
                                                    Model removedModel, String context) {
        final Set<Statement> statements = new HashSet<>(toEnhance);
        statements.addAll(addedModel.listStatements(subject, property, value).toList());
        removedModel.listStatements(subject, property, value).toList().forEach(statements::remove);
        return statements.stream()
                         .filter(s -> removedSubjectPredicateStatements.stream()
                                                                       .noneMatch(spc -> spc.matches(s, context)))
                         .collect(Collectors.toSet());
    }

    Containment contains(Resource subject, Property property, RDFNode value, Collection<String> contexts) {
        if (contexts.isEmpty()) {
            if (removedDefault().contains(subject, property, value) || removedSubjectPredicateStatements.contains(new SubjectPredicateContext(subject, property, Collections.emptySet()))) {
                return Containment.REMOVED;
            } else {
                return addedDefault().contains(subject, property, value) ? Containment.ADDED :
                        Containment.UNKNOWN;
            }
        } else {
            Containment result = Containment.UNKNOWN;
            if (removedSubjectPredicateStatements.contains(new SubjectPredicateContext(subject, property, new HashSet<>(contexts)))) {
                return Containment.REMOVED;
            }
            for (String c : contexts) {
                if (removed.getNamedModel(c).contains(subject, property, value)) {
                    return Containment.REMOVED;
                } else if (added.getNamedModel(c).contains(subject, property, value)) {
                    result = Containment.ADDED;
                }
            }
            return result;
        }
    }

    void addStatements(List<Statement> statements, String context) {
        registerStatements(statements, context, added, removed);
    }

    private void registerStatements(List<Statement> statements, String context, Dataset addTo, Dataset removeFrom) {
        if (context != null) {
            addTo.getNamedModel(context).add(statements);
            removeFrom.getNamedModel(context).remove(statements);
        } else {
            addTo.getDefaultModel().add(statements);
            removeFrom.getDefaultModel().remove(statements);
            if (defaultAsUnion) {
                removeFrom.listNames().forEachRemaining(n -> removeFrom.getNamedModel(n).remove(statements));
            }
        }
    }

    void removeStatements(List<Statement> statements, String context) {
        registerStatements(statements, context, removed, added);
    }

    void removeStatementsBySubjectAndPredicate(Collection<SubjectPredicateContext> toRemove) {
        removedSubjectPredicateStatements.addAll(toRemove);
    }

    Dataset getAdded() {
        return added;
    }

    Dataset getRemoved() {
        return removed;
    }

    public Set<SubjectPredicateContext> getRemovedSubjectPredicateStatements() {
        return removedSubjectPredicateStatements;
    }

    List<String> getContexts() {
        final Iterator<String> it = added.listNames();
        final List<String> contexts = new ArrayList<>();
        it.forEachRemaining(contexts::add);
        return contexts;
    }
}
