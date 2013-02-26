package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * Defines common interface for all transactional resources.
 * 
 * @author kidney
 * 
 */
public interface Transactional extends Closeable {

	/**
	 * {@inheritDoc}
	 * 
	 * Closing this resource discards all pending changes on this resource.
	 */
	public void close() throws OntoDriverException;

	/**
	 * Makes all pending changes persistent within this resource. </p>
	 * 
	 * A new transaction is started automatically when the resource is opened
	 * and after calling the {@code commit} or {@code rollback}.
	 * 
	 * @throws OntoDriverException
	 *             If an ontology error occurs
	 * @see #rollback()
	 */
	public void commit() throws OntoDriverException;

	/**
	 * Discards all pending changes. </p>
	 * 
	 * @throws OntoDriverException
	 *             If an ontology error occurs
	 * @see #commit()
	 */
	public void rollback() throws OntoDriverException;

}
