/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Model;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.impl.LinkedHashModel;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Caches local transactional changes to the RDF4J repository model.
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

    List<Statement> enhanceStatements(Stream<Statement> statements, Resource subject, IRI property,
                                      Value object, Collection<IRI> context) {
        final IRI[] ctxArray = context.toArray(new IRI[0]);
        final Collection<Statement> added = addedStatements.filter(subject, property, object, ctxArray);
        final Collection<Statement> removed = removedStatements.filter(subject, property, object, ctxArray);
        final List<Statement> result = statements.filter(s -> !removed.contains(s)).collect(Collectors.toList());
        result.addAll(added);
        return result;
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
