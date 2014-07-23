package cz.cvut.kbss.ontodriver;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
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
	 * primary keys are unique. </p>
	 * 
	 * We are using identity map, because counters are
	 */
	protected static AtomicInteger primaryKeyCounter = new AtomicInteger();

	/** Backward reference to the factory */
	protected final DriverFactory factory;
	/** Metamodel of the entity model */
	protected final PersistenceProviderFacade persistenceProvider;
	/** True if this module is open */
	protected boolean open;
	protected TransactionState transaction;

	public StorageModule(PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		this.persistenceProvider = Objects.requireNonNull(persistenceProvider,
				"Argument 'persistenceProvider' cannot be null.");
		this.factory = Objects.requireNonNull(factory, "Argument 'factory' cannot be null.");

		initialize();
		this.open = true;
		this.transaction = TransactionState.NO;
	}

	@Override
	public void close() throws OntoDriverException {
		factory.releaseStorageModule(this);
		this.open = false;
		this.transaction = TransactionState.NO;
	}

	/**
	 * Gets metamodel associated with this storage module.
	 * 
	 * @return Metamodel
	 */
	public Metamodel getMetamodel() {
		return persistenceProvider.getMetamodel();
	}

	/**
	 * Gets new primary key counter and increments the internal counter.
	 * 
	 * @return Primary key counter
	 */
	public int getNewPrimaryKey() {
		return getNewPrimaryKeyImpl();
	}

	/**
	 * Tries to get entity from the persistence provider's second level cache.
	 * </p>
	 * 
	 * The specified repository ID is used to determine contexts to search.
	 * 
	 * @param cls
	 *            Entity class
	 * @param primaryKey
	 *            Entity primary key
	 * @param descriptor
	 *            Entity descriptor
	 * @return Matching entity or {@code null}
	 */
	public <T> T getEntityFromProviderCache(Class<T> cls, Object primaryKey,
			EntityDescriptor descriptor) {
		assert cls != null;
		assert primaryKey != null;
		assert descriptor != null;

		return persistenceProvider.getEntityFromLiveObjectCache(cls, primaryKey,
				descriptor.getEntityContext());
	}

	/**
	 * Increments the primary key counter for this module's context.
	 */
	public void incrementPrimaryKeyCounter() {
		incrementPrimaryKeyCounterImpl();
	}

	/**
	 * Gets logical URI of the underlying ontology. </p>
	 * 
	 * Note that the logical URI may be {@code null}, e. g. for Sesame
	 * repositories.
	 * 
	 * @return Logical URI or {@code null}
	 */
	public URI getOntologyUri() {
		return factory.getOntologyUri();
	}

	/**
	 * Gets physical URI of the underlying ontology. </p>
	 * 
	 * @return Physical URI
	 */
	public URI getPhysicalUri() {
		return factory.getPhysicalUri();
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
	 * The context URI parameter may be null, indicating that the whole
	 * repository should be searched.
	 * 
	 * @param primaryKey
	 *            Primary key
	 * @param context
	 *            Context which should be searched
	 * @return {@code true} if this module contains entity with the specified
	 *         primary key, {@code false} otherwise
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code repository} is {@code null}
	 */
	public abstract boolean contains(Object primaryKey, URI context) throws OntoDriverException;

	/**
	 * Retrieves entity with the specified primary key. </p>
	 * 
	 * @param cls
	 *            Type of the returned entity
	 * @param primaryKey
	 *            Primary key
	 * @param descriptor
	 *            Entity descriptor specifying contexts in which the entity and
	 *            its field values should be searched for
	 * @return Found entity or null
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code cls}, {@code primaryKey} or {@code descriptor} is
	 *             {@code null}
	 */
	public abstract <T> T find(Class<T> cls, Object primaryKey, EntityDescriptor descriptor)
			throws OntoDriverException;

	/**
	 * Gets an unmodifiable list of contexts available to this storage module.
	 * </p>
	 * 
	 * @return Unmodifiable list of context URIs
	 */
	public abstract List<URI> getContexts();

	/**
	 * Checks whether the underlying ontology context is consistent. </p>
	 * 
	 * If the context URI is null, consistency of the whole repository is
	 * checked.
	 * 
	 * @param context
	 *            Context to validate
	 * @return {@code true} if the contexts are consistent, {@code false}
	 *         otherwise
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             if {@code contexts} is {@code null}
	 */
	public abstract boolean isConsistent(URI context) throws OntoDriverException;

	/**
	 * Loads from the ontology and sets value of field {@code fieldName} on the
	 * specified entity. </p>
	 * 
	 * @param entity
	 *            The entity to set field value on
	 * @param field
	 *            The field to load
	 * @param descriptor
	 *            Entity descriptor
	 * @throws OntoDriverException
	 * 
	 *             If called on a closed storage module, if the field name does
	 *             not exist or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code entity}, {@code fieldName} or {@code descriptor} is
	 *             null
	 */
	public abstract <T> void loadFieldValue(T entity, Field field, EntityDescriptor descriptor)
			throws OntoDriverException;

	/**
	 * Merges changes on the specified entity field into this module. </p>
	 * 
	 * @param entity
	 *            The entity to merge
	 * @param mergedField
	 *            The field to merge
	 * @param descriptor
	 *            Entity descriptor specifying into which context the value will
	 *            be merged
	 * @throws OntoDriverException
	 *             If the entity does not exist in this module or if an ontology
	 *             access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey}, {@code entity} or {@code descriptor}
	 *             is {@code null}
	 */
	public abstract <T> void merge(T entity, Field mergedField, EntityDescriptor descriptor)
			throws OntoDriverException;

	/**
	 * Persists the specified entity into this module. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the entity. Optional, if not set it will be
	 *            generated
	 * @param entity
	 *            The entity to persist
	 * @param descriptor
	 *            Repository identifier specifying into which contexts the
	 *            entity will be persisted and its fields will be
	 * @throws OntoDriverException
	 *             If an entity with the specified primary key already exists in
	 *             this module or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code entity} or {@code descriptor} is null
	 */
	public abstract <T> void persist(Object primaryKey, T entity, EntityDescriptor descriptor)
			throws OntoDriverException;

	/**
	 * Removes entity with the specified primary key from this module. </p>
	 * 
	 * If there is no entity with {@code primaryKey} then this method does
	 * nothing.
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to remove
	 * @param descriptor
	 *            Repository identifier specifying from which context the entity
	 *            will be removed
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code context} is null
	 */
	public abstract void remove(Object primaryKey, EntityDescriptor descriptor)
			throws OntoDriverException;

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
	 * Preliminary steps before running {@link #contains(Object, URI)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected void preContains(Object primaryKey, URI context) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running
	 * {@link #find(Class, Object, EntityDescriptor)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected void preFind(Class<?> cls, Object primaryKey, EntityDescriptor descriptor)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running {@link #isConsistent(URI)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected void preIsConsistent(URI context) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running
	 * {@link #loadFieldValue(Object, Field, EntityDescriptor)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected <T> void preLoadFieldValue(T entity, Field field, EntityDescriptor descriptor)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(field, ErrorUtils.constructNPXMessage("field"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running
	 * {@link #merge(Object, Object, Field, EntityDescriptor)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected <T> void preMerge(T entity, Field mergedField, EntityDescriptor descriptor)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(mergedField, ErrorUtils.constructNPXMessage("mergedField"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running
	 * {@link #persist(Object, Object, EntityDescriptor)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected <T> void prePersist(T entity, EntityDescriptor descriptor) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running
	 * {@link #remove(Object, EntityDescriptor)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected void preRemove(Object primaryKey, EntityDescriptor descriptor)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));
		startTransactionIfNotActive();
	}

	/**
	 * Ensures that this module is in valid state.
	 * 
	 * @throws IllegalStateException
	 *             If not in valid state
	 */
	protected void ensureOpen() {
		if (!open) {
			throw new IllegalStateException("The storage module is closed.");
		}
	}

	/**
	 * Ensures that an internal transaction is active.
	 * 
	 * @throws IllegalStateException
	 *             If transaction is not active
	 */
	protected void ensureTransactionActive() {
		if (transaction != TransactionState.ACTIVE) {
			throw new IllegalStateException("No internal transaction is active!");
		}
	}

	/**
	 * Sets the initial value of the primary key counter.
	 * 
	 * @param initValue
	 *            The initial value, greater or equal to 0
	 */
	protected static void setPrimaryKeyCounter(int initValue) {
		assert initValue >= 0;
		primaryKeyCounter = new AtomicInteger(initValue);
	}

	/**
	 * Retrieves a new primary key number and increments the internal counter.
	 * 
	 * @param ctx
	 *            The context from which key should be retrieved
	 * @return primary key number
	 */
	protected static int getNewPrimaryKeyImpl() {
		return primaryKeyCounter.incrementAndGet();
	}

	/**
	 * Increments the primary key counter in the specified context.
	 * 
	 * @param ctx
	 *            Context
	 */
	protected static void incrementPrimaryKeyCounterImpl() {
		getNewPrimaryKeyImpl();
	}
}
