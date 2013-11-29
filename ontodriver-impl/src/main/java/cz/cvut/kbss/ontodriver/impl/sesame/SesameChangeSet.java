package cz.cvut.kbss.ontodriver.impl.sesame;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.openrdf.model.Statement;

/**
 * Contains changes committed in a transaction. </p>
 * 
 * These changes are represented by statements that will be added/removed
 * to/from the repository.
 * 
 * @author ledvima1
 * 
 */
final class SesameChangeSet {

	private final List<Statement> add;
	private final List<Statement> remove;

	SesameChangeSet() {
		this.add = new ArrayList<>();
		this.remove = new ArrayList<>();
	}

	/**
	 * Adds the specified statements to those that will be added to the
	 * repository.
	 * 
	 * @param stmt
	 *            The statement to add
	 */
	void add(Statement stmt) {
		assert stmt != null;
		add.add(stmt);
	}

	/**
	 * Adds the specified statement to those that will be removed from the
	 * repository.
	 * 
	 * @param stmt
	 *            The statement to remove
	 */
	void remove(Statement stmt) {
		assert stmt != null;
		remove.add(stmt);
	}

	/**
	 * Gets statements that should be added to the repository. </p>
	 * 
	 * The list cannot be modified.
	 * 
	 * @return Statements to add
	 */
	List<Statement> getAdd() {
		return Collections.unmodifiableList(add);
	}

	/**
	 * Gets statements that should be removed from the repository. </p>
	 * 
	 * The list cannot be modified.
	 * 
	 * @return Statements to remove
	 */
	List<Statement> getRemove() {
		return Collections.unmodifiableList(remove);
	}
}
