/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import org.openrdf.model.*;
import org.openrdf.model.impl.LinkedHashModel;

import java.util.Collection;

/**
 * Caches local transactional changes to the Sesame repository model.
 * 
 * @author ledvima1
 * 
 */
class LocalModel {

	private final Model addedStatements;
	private final Model removedStatements;

	LocalModel() {
		this.addedStatements = new LinkedHashModel();
		this.removedStatements = new LinkedHashModel();
	}

	void enhanceStatements(Collection<Statement> statements, Resource subject, URI property,
			Value object, URI... contexts) {
		final URI[] ctxs = contexts != null ? contexts : new URI[0];
		final Collection<Statement> added = addedStatements.filter(subject, property, object, ctxs);
		statements.addAll(added);
		final Collection<Statement> removed = removedStatements.filter(subject, property, object, ctxs);
		statements.removeAll(removed);
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
