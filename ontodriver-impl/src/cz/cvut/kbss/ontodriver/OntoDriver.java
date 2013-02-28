package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * Represents driver which controls and provides access to ontology storages.
 * </p>
 * 
 * The driver provides {@code StorageManager} objects that provide direct access
 * to the underlying storage and are requested by incoming {@code Connection}s.
 * 
 * @author kidney
 * 
 */
public interface OntoDriver {

	/**
	 * Acquires storage manager for the specified connection. </p>
	 * 
	 * Storage managers control and provide access to the underlying storage.
	 * The implementation of {@code OntoDriver} can create them on demand for
	 * each {@code Connection} or can keep a pool of them (similar to JPA's
	 * DataSource).
	 * 
	 * @return {@code StorageManager}
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 */
	public StorageManager acquireStorageManager() throws OntoDriverException;

	/**
	 * Acquires storage manager for the specified connection. </p>
	 * 
	 * Storage managers control and provide access to the underlying storage.
	 * The implementation of {@code OntoDriver} can create them on demand for
	 * each {@code Connection} or can keep a pool of them (similar to JPA's
	 * DataSource).
	 * 
	 * @param persistenceProvider
	 *            Facade representing the persistence provider
	 * @return {@code StorageManager}
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 */
	public StorageManager acquireStorageManager(PersistenceProviderFacade persistenceProvider)
			throws OntoDriverException;
}
