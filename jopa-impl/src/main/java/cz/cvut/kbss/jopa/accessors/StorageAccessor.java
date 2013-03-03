package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.ontodriver.Connection;

public interface StorageAccessor {

	/**
	 * Acquires a connection to the underlying ontology driver. </p>
	 * 
	 * @return Connection to the storage
	 * @throws OWLPersistenceException
	 *             If an error occurs during storage access
	 */
	public Connection acquireConnection();
}
