package cz.cvut.kbss.ontodriver;

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
public abstract class StorageModule implements Transactional {

	/** Context information */
	protected final Context context;
	/** True if this module is open */
	protected boolean open;

	public StorageModule(Context context) {
		if (context == null) {
			throw new NullPointerException("Context cannot be null.");
		}
		this.context = context;
		initialize();
		this.open = true;
	}

	@Override
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

	@Override
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
	public abstract <T> T find(Class<T> cls, Object primaryKey)
			throws OntoDriverException;

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
	public abstract <T> void merge(Object primaryKey, T entity)
			throws OntoDriverException;

	/**
	 * Persists the specified entity into this module. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the entity. Optional, if not set it will be
	 *            generated
	 * @param entity
	 *            The entity to persist
	 * @throws OntoDriverException
	 *             If an entity with the specified primary key already exists in
	 *             this module or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code entity} is null
	 */
	public abstract <T> void persist(Object primaryKey, T entity)
			throws OntoDriverException;

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
	public abstract ResultSet executeStatement(Statement statement)
			throws OntoDriverException;
}
