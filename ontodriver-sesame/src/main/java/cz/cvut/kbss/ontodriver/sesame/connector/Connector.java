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
	 * Explicitly starts a transaction.
	 * 
	 * @throws SesameDriverException
	 *             If unable to start transaction
	 */
	public void begin() throws SesameDriverException;

	/**
	 * Commits the changes made since transaction beginning. </p>
	 * 
	 * @throws SesameDriverException
	 *             If an error occurs during commit
	 * @see #begin()
	 */
	public void commit() throws SesameDriverException;

	/**
	 * Rolls back changes made since transaction beginning. </p>
	 * 
	 * @throws SesameDriverException
	 *             If an error occurs when rolling back
	 * @see #begin()
	 */
	public void rollback() throws SesameDriverException;

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
	 * @throws IllegalStateException
	 *             If transaction is not active
	 * @throws SesameDriverException
	 *             If a repository access error occurs
	 */
	public void addStatements(Collection<Statement> statements) throws SesameDriverException;

	/**
	 * Removes the specified statements from the underlying repository. </p>
	 * 
	 * Note that this operation is transactional and the changes are required to
	 * be persistent only after successful {@link #commit()}.
	 * 
	 * @param statements
	 *            The statements to remove
	 * @throws IllegalStateException
	 *             If transaction is not active
	 * @throws SesameDriverException
	 *             If a repository access error occurs
	 */
	public void removeStatements(Collection<Statement> statements) throws SesameDriverException;
}
