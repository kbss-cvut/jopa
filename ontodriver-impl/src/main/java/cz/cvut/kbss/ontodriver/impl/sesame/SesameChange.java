package cz.cvut.kbss.ontodriver.impl.sesame;

import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;

interface SesameChange {

	/**
	 * Applies this change to repository represented by the specified
	 * connection.
	 * 
	 * @param connection
	 *            Repository connection
	 */
	void apply(RepositoryConnection connection) throws RepositoryException;

}
