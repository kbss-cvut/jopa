/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
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
                           Value object, IRI context) {
        final Collection<Statement> added, removed;
        if (context != null) {
            added = addedStatements.filter(subject, property, object, context);
            removed = removedStatements.filter(subject, property, object, context);
        } else {
            added = addedStatements.filter(subject, property, object);
            removed = removedStatements.filter(subject, property, object);
        }
        statements.addAll(added);
        statements.removeAll(removed);
    }

    Contains contains(Resource subject, IRI property, Value object, IRI context) {
        if (context != null) {
            if (addedStatements.contains(subject, property, object, context)) {
                return Contains.TRUE;
            }
            return removedStatements.contains(subject, property, object, context) ? Contains.FALSE : Contains.UNKNOWN;
        } else {
            if (addedStatements.contains(subject, property, object)) {
                return Contains.TRUE;
            }
            return removedStatements.contains(subject, property, object) ? Contains.FALSE : Contains.UNKNOWN;
        }
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
