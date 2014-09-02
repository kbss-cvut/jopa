package cz.cvut.kbss.ontodriver.sesame.connector;

import java.util.Collection;
import java.util.List;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.Closeable;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;

public interface Connector extends Closeable, StatementExecutor {

	/**
	 * Commits the changes made since transaction beginning. </p>
	 * 
	 * Transaction beginning can be either creating of this Connector or the
	 * last commit/rollback call.
	 */
	public void commit();

	/**
	 * Rolls back changes made since transaction beginning. </p>
	 * 
	 * Transaction beginning can be either creating of this Connector or the
	 * last commit/rollback call.
	 */
	public void rollback();

	/**
	 * Gets resources representing currently existing contexts in the
	 * repository.
	 * 
	 * @return List of resources
	 * @throws SesameDriverException
	 *             If repository access error occurs
	 */
	public List<Resource> getContexts() throws SesameDriverException;

	/**
	 * Gets Sesame value factory.
	 * 
	 * @return {@link ValueFactory}
	 */
	public ValueFactory getValueFactory();

	/**
	 * Adds the specified statements to the underlying repository. </p>
	 * 
	 * Note that this operation is transactional and the changes are required to
	 * be persistent only after successful {@link #commit()}.
	 * 
	 * @param statements
	 *            The statements to add
	 */
	public void addStatements(Collection<Statement> statements);
}
