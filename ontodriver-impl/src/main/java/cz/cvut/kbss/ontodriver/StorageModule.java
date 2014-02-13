package cz.cvut.kbss.ontodriver;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

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

	/**
	 * Denotes in which transactional state this module currently is.
	 * 
	 * @author kidney
	 * 
	 */
	public static enum TransactionState {
		NO, ACTIVE, COMMIT
	};

	/**
	 * Counters that increment with each inserted entity so that newly generated
	 * primary keys are unique
	 */
	protected static final Map<Context, AtomicInteger> primaryKeyCounters = new HashMap<Context, AtomicInteger>();

	/** Context information */
	protected final Context context;
	/** Backward reference to the factory */
	protected final DriverFactory factory;
	/** Metamodel of the entity model */
	protected final PersistenceProviderFacade persistenceProvider;
	/** True if this module is open */
	protected boolean open;
	protected TransactionState transaction;

	public StorageModule(Context context, PersistenceProviderFacade persistenceProvider,
			DriverFactory factory) throws OntoDriverException {
		if (context == null) {
			throw new NullPointerException("Context cannot be null.");
		}
		if (persistenceProvider == null) {
			throw new NullPointerException("PersistenceProvider cannot be null.");
		}
		if (factory == null) {
			throw new NullPointerException("Factory cannot be null.");
		}
		this.context = context;
		this.persistenceProvider = persistenceProvider;
		this.factory = factory;
		initialize();
		this.open = true;
		this.transaction = TransactionState.NO;
	}

	@Override
	public void close() throws OntoDriverException {
		this.open = false;
		this.transaction = TransactionState.NO;
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
	 * Gets metamodel associated with this storage module.
	 * 
	 * @return Metamodel
	 */
	public Metamodel getMetamodel() {
		return persistenceProvider.getMetamodel();
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
	 * 
	 * @throws OntoDriverException
	 */
	protected abstract void initialize() throws OntoDriverException;

	/**
	 * Resolves whether this module contains entity with the specified primary
	 * key. </p>
	 * 
	 * @param primaryKey
	 *            Primary key
	 * @return {@code true} if this module contains entity with the specified
	 *         primary key, {@code false} otherwise
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} is {@code null}
	 */
	public abstract boolean contains(Object primaryKey) throws OntoDriverException;

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
	 *             If {@code cls} or {@code primaryKey} is {@code null}
	 */
	public abstract <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException;

	/**
	 * Checks whether the underlying ontology context is consistent.
	 * 
	 * @return {@code true} if the context is consistent, {@code false}
	 *         otherwise
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 */
	public abstract boolean isConsistent() throws OntoDriverException;

	/**
	 * Loads from the ontology and sets value of field {@code fieldName}. </p>
	 * 
	 * This method is intended to be used for lazy loaded field values.
	 * 
	 * @param entity
	 *            The entity to set field value on
	 * @param field
	 *            The field to load
	 * @throws OntoDriverException
	 *             If called on a closed storage module, if the field name does
	 *             not exist or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code entity} or {@code fieldName} is null
	 */
	public abstract <T> void loadFieldValue(T entity, Field field) throws OntoDriverException;

	/**
	 * Merges changes on the specified entity field into this module. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the entity
	 * @param entity
	 *            The entity to merge
	 * @param mergedField
	 *            The field to merge
	 * @throws OntoDriverException
	 *             If the entity does not exist in this module or if an ontology
	 *             access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code entity} is null
	 */
	public abstract <T> void merge(Object primaryKey, T entity, Field mergedField)
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
	public abstract ResultSet executeStatement(JopaStatement statement) throws OntoDriverException;

	/**
	 * Starts an internal transaction if one is not already active. </p>
	 * 
	 * When an internal transaction is started an ontology snapshot should be
	 * created and used during the transaction.
	 * 
	 * @throws OntoDriverException
	 *             If an error during transaction start occurs
	 */
	protected abstract void startTransactionIfNotActive() throws OntoDriverException;

	/**
	 * Ensures that this module is in valid state.
	 * 
	 * @throws OntoDriverException
	 *             If not in valid state
	 */
	protected void ensureOpen() throws OntoDriverException {
		if (!open) {
			throw new OntoDriverException(
					new IllegalStateException("The storage module is closed."));
		}
	}

	/**
	 * Ensures that an internal transaction is active.
	 * 
	 * @throws OntoDriverException
	 *             If transaction is not active
	 */
	protected void ensureTransactionActive() throws OntoDriverException {
		if (transaction != TransactionState.ACTIVE) {
			throw new OntoDriverException(new IllegalStateException(
					"No internal transaction is active!"));
		}
	}

	/**
	 * Gets new primary key counter and increments the internal counter.
	 * 
	 * @return Primary key counter
	 */
	public int getNewPrimaryKey() {
		return StorageModule.getNewPrimaryKey(context);
	}

	/**
	 * Increments the primary key counter for this module's context.
	 */
	public void incrementPrimaryKeyCounter() {
		StorageModule.incrementPrimaryKeyCounter(context);
	}

	/**
	 * Retrieves a new primary key number and increments the internal counter.
	 * 
	 * @param ctx
	 *            The context from which key should be retrieved
	 * @return primary key number
	 */
	protected static int getNewPrimaryKey(Context ctx) {
		assert primaryKeyCounters.containsKey(ctx);
		return primaryKeyCounters.get(ctx).incrementAndGet();
	}

	/**
	 * Increments the primary key counter in the specified context.
	 * 
	 * @param ctx
	 *            Context
	 */
	protected static void incrementPrimaryKeyCounter(Context ctx) {
		assert primaryKeyCounters.containsKey(ctx);
		primaryKeyCounters.get(ctx).incrementAndGet();
	}
}
