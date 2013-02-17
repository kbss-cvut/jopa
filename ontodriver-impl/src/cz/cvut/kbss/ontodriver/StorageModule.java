package cz.cvut.kbss.ontodriver;

import java.net.URI;

/**
 * Represents a single storage module. </p>
 * 
 * A storage module in this case can be either an OWL module, an RDF named graph
 * (or a part of it) or a single ontology (which can be both of the former
 * concepts).
 * 
 * @author kidney
 * 
 */
public abstract class StorageModule {

	/** Context information */
	protected final Context context;
	/** True if this module is open */
	protected boolean open;

	public StorageModule(URI ontologyUri) {
		this.context = new Context(ontologyUri);
		initialize();
		this.open = true;
	}

	/**
	 * Closes this storage module. </p>
	 * 
	 * By closing the module, the connection to the underlying physical storage
	 * is released and cannot be reaquired. </p>
	 * 
	 * Closing an already closed storage module does nothing.
	 */
	public void close() {
		this.open = false;
	}

	/**
	 * Retrieves context of this storage module.
	 * 
	 * @return Context
	 */
	public Context getContext() {
		return context;
	}

	/**
	 * Returns true if this storage module is open.
	 * 
	 * @return True if open false otherwise
	 * @see #close()
	 */
	public boolean isOpen() {
		return open;
	}

	/**
	 * Initializes this StorageModule. </p>
	 * 
	 * This means setting information about its expressiveness and signature and
	 * acquiring connection to the physical storage.
	 */
	protected abstract void initialize();

	/**
	 * Makes the pending changes in this module persistent. </p>
	 * 
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 */
	public abstract void commit() throws OntoDriverException;

	/**
	 * Rolls back all pending changes in this module. </p>
	 * 
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 */
	public abstract void rollback() throws OntoDriverException;

	/**
	 * Retrieves entity with the specified primary key. </p>
	 * 
	 * 
	 * @param cls
	 *            Type of the returned entity
	 * @param primaryKey
	 *            Primary key
	 * @return Found entity or null
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code cls} or {@code primaryKey} is null
	 */
	public abstract <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException;

	/**
	 * Merges changes on the specified entity into this module. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the entity
	 * @param entity
	 *            The entity to merge
	 * @throws OntoDriverException
	 *             If the entity does not exist in this module or if an ontology
	 *             access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code entity} is null
	 */
	public abstract <T> void merge(Object primaryKey, T entity) throws OntoDriverException;

	/**
	 * Persists the specified entity into this module. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the entity
	 * @param entity
	 *            The entity to persist
	 * @throws OntoDriverException
	 *             If an entity with the specified primary key already exists in
	 *             this module or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code entity} is null
	 */
	public abstract <T> void persist(Object primaryKey, T entity) throws OntoDriverException;

	/**
	 * Removes entity with the specified primary key from this module. </p>
	 * 
	 * If there is no entity with {@code primaryKey} then this method does
	 * nothing.
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to remove
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} is null
	 */
	public abstract void remove(Object primaryKey) throws OntoDriverException;

	/**
	 * Executes the specified statement. </p>
	 * 
	 * If the statement is an update the number of affected entities is returned
	 * in the result set.
	 * 
	 * @param statement
	 *            The statement to execute
	 * @return Result of the execution
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code statement} is null
	 */
	public abstract ResultSet executeStatement(Statement statement) throws OntoDriverException;
}
