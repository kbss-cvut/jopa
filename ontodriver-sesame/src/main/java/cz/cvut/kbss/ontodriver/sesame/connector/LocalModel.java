/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame.connector;

import org.eclipse.rdf4j.model.*;
import org.eclipse.rdf4j.model.impl.LinkedHashModel;

import java.util.Collection;

/**
 * Caches local transactional changes to the Sesame repository model.
 */
class LocalModel {

    private final Model addedStatements;
    private final Model removedStatements;

    enum Contains {
        TRUE, FALSE, UNKNOWN
    }

    LocalModel() {
        this.addedStatements = new LinkedHashModel();
        this.removedStatements = new LinkedHashModel();
    }

    void enhanceStatements(Collection<Statement> statements, Resource subject, IRI property,
                           Value object, Collection<IRI> context) {
        final Collection<Statement> added, removed;
        final IRI[] ctxArray = context.toArray(new IRI[0]);
        added = addedStatements.filter(subject, property, object, ctxArray);
        removed = removedStatements.filter(subject, property, object, ctxArray);
        statements.addAll(added);
        statements.removeAll(removed);
    }

    Contains contains(Resource subject, IRI property, Value object, Collection<IRI> contexts) {
        final IRI[] ctxArray = contexts.toArray(new IRI[0]);
        if (addedStatements.contains(subject, property, object, ctxArray)) {
            return Contains.TRUE;
        }
        return removedStatements.contains(subject, property, object, ctxArray) ? Contains.FALSE : Contains.UNKNOWN;
    }

    void addStatements(Collection<Statement> statements) {
        removedStatements.removeAll(statements);
        addedStatements.addAll(statements);
    }

    void removeStatements(Collection<Statement> statements) {
        addedStatements.removeAll(statements);
        removedStatements.addAll(statements);
    }

    Collection<Statement> getAddedStatements() {
        return addedStatements;
    }

    Collection<Statement> getRemovedStatements() {
        return removedStatements;
    }
}
