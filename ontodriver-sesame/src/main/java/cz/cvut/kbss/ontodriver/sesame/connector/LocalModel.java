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
