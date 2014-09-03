package cz.cvut.kbss.ontodriver.sesame.connector;

import java.util.Collection;

import org.openrdf.model.Model;
import org.openrdf.model.Statement;
import org.openrdf.model.impl.LinkedHashModel;

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

	void enhanceModel(Model model) {
		// TODO

	}

	void addStatements(Collection<Statement> statements) {
		addedStatements.addAll(statements);
	}

	void removeStatements(Collection<Statement> statements) {
		addedStatements.removeAll(statements);
		this.removedStatements.addAll(statements);
	}

	Collection<Statement> getAddedStatements() {
		return addedStatements;
	}

	Collection<Statement> getRemovedStatements() {
		return removedStatements;
	}
}
