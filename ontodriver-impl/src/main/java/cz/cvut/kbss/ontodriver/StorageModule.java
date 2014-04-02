package cz.cvut.kbss.ontodriver;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
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
	 * Keys in this map are internal identifiers of repositories.
	 */
	protected static final Map<Integer, AtomicInteger> primaryKeyCounters = new HashMap<Integer, AtomicInteger>();

	/** Repository managed by this module */
	protected final Repository repository;
	/** Cached repository identifier */
	protected final RepositoryID repositoryId;
	/** Backward reference to the factory */
	protected final DriverFactory factory;
	/** Metamodel of the entity model */
	protected final PersistenceProviderFacade persistenceProvider;
	/** True if this module is open */
	protected boolean open;
	protected TransactionState transaction;

	public StorageModule(Repository repository, PersistenceProviderFacade persistenceProvider,
			DriverFactory factory) throws OntoDriverException {
		this.repository = Objects.requireNonNull(repository,
				"Argument 'repository' cannot be null.");
		this.persistenceProvider = Objects.requireNonNull(persistenceProvider,
				"Argument 'persistenceProvider' cannot be null.");
		this.factory = Objects.requireNonNull(factory, "Argument 'factory' cannot be null.");
		this.repositoryId = repository.createRepositoryID(false);

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
	 * Retrieves repository represented by this module.
	 * 
	 * @return repository
	 */
	public Repository getRepository() {
		return repository;
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
		return StorageModule.getNewPrimaryKey(repository);
	}

	/**
	 * Increments the primary key counter for this module's context.
	 */
	public void incrementPrimaryKeyCounter() {
		StorageModule.incrementPrimaryKeyCounter(repository);
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
	 * @param contexts
	 *            Repository identifier. It is here merely to specify contexts
	 *            which should be searched
	 * @return {@code true} if this module contains entity with the specified
	 *         primary key, {@code false} otherwise
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code contexts} is {@code null}
	 */
	public abstract boolean contains(Object primaryKey, RepositoryID contexts)
			throws OntoDriverException;

	/**
	 * Retrieves entity with the specified primary key. </p>
	 * 
	 * Multiple contexts can be specified to be searched.
	 * 
	 * @param cls
	 *            Type of the returned entity
	 * @param primaryKey
	 *            Primary key
	 * @param contexts
	 *            Repository identifier. It is here merely to specify contexts
	 *            which should be searched
	 * @return Found entity or null
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code cls}, {@code primaryKey} or {@code contexts} is
	 *             {@code null}
	 */
	public abstract <T> T find(Class<T> cls, Object primaryKey, RepositoryID contexts)
			throws OntoDriverException;

	/**
	 * Checks whether the underlying ontology contexts are consistent.
	 * 
	 * @param contexts
	 *            Contexts to validate
	 * @return {@code true} if the contexts are consistent, {@code false}
	 *         otherwise
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             if {@code contexts} is {@code null}
	 */
	public abstract boolean isConsistent(RepositoryID contexts) throws OntoDriverException;

	/**
	 * Loads from the ontology and sets value of field {@code fieldName} on the
	 * specified entity. </p>
	 * 
	 * @param entity
	 *            The entity to set field value on
	 * @param field
	 *            The field to load
	 * @param contexts
	 *            Repository identifier specifying contexts to search
	 * @throws OntoDriverException
	 * 
	 *             If called on a closed storage module, if the field name does
	 *             not exist or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code entity}, {@code fieldName} or {@code contexts} is
	 *             null
	 */
	public abstract <T> void loadFieldValue(T entity, Field field, RepositoryID contexts)
			throws OntoDriverException;

	/**
	 * Merges changes on the specified entity field into this module. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the entity
	 * @param entity
	 *            The entity to merge
	 * @param mergedField
	 *            The field to merge
	 * @param context
	 *            Repository identifier specifying into which context the value
	 *            will be merged. If the identifier contains multiple contexts,
	 *            the first one returned by iterator is used
	 * @throws OntoDriverException
	 *             If the entity does not exist in this module or if an ontology
	 *             access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey}, {@code entity} or {@code context} is
	 *             {@code null}
	 */
	public abstract <T> void merge(Object primaryKey, T entity, Field mergedField,
			RepositoryID context) throws OntoDriverException;

	/**
	 * Persists the specified entity into this module. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the entity. Optional, if not set it will be
	 *            generated
	 * @param entity
	 *            The entity to persist
	 * @param context
	 *            Repository identifier specifying into which context the entity
	 *            will be persisted. If the identifier contains multiple
	 *            contexts, the first one returned by iterator is used
	 * @throws OntoDriverException
	 *             If an entity with the specified primary key already exists in
	 *             this module or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code entity} or {@code context} is null
	 */
	public abstract <T> void persist(Object primaryKey, T entity, RepositoryID context)
			throws OntoDriverException;

	/**
	 * Removes entity with the specified primary key from this module. </p>
	 * 
	 * If there is no entity with {@code primaryKey} then this method does
	 * nothing.
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to remove
	 * @param context
	 *            Repository identifier specifying from which context the entity
	 *            will be removed. If the identifier contains multiple contexts,
	 *            the first one returned by iterator is used
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code context} is null
	 */
	public abstract void remove(Object primaryKey, RepositoryID context) throws OntoDriverException;

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
	 * Preliminary steps before running {@link #contains(Object, RepositoryID)}
	 * </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected void preContains(Object primaryKey, RepositoryID contexts) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(contexts, ErrorUtils.constructNPXMessage("contexts"));
		ensureCorrectRepositoryId(contexts);
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running
	 * {@link #find(Class, Object, RepositoryID)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected void preFind(Class<?> cls, Object primaryKey, RepositoryID contexts)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(contexts, ErrorUtils.constructNPXMessage("contexts"));
		ensureCorrectRepositoryId(contexts);
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running {@link #isConsistent(RepositoryID)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected void preIsConsistent(RepositoryID contexts) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(contexts, ErrorUtils.constructNPXMessage("contexts"));
		ensureCorrectRepositoryId(contexts);
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running
	 * {@link #loadFieldValue(Object, Field, RepositoryID)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected <T> void preLoadFieldValue(T entity, Field field, RepositoryID context)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(field, ErrorUtils.constructNPXMessage("field"));
		Objects.requireNonNull(context, ErrorUtils.constructNPXMessage("context"));
		ensureCorrectRepositoryId(context);
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running
	 * {@link #merge(Object, Object, Field, RepositoryID)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected <T> void preMerge(Object primaryKey, T entity, Field mergedField, RepositoryID context)
			throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(mergedField, ErrorUtils.constructNPXMessage("mergedField"));
		Objects.requireNonNull(context, ErrorUtils.constructNPXMessage("context"));
		ensureCorrectRepositoryId(context);
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running
	 * {@link #persist(Object, Object, RepositoryID)} </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected <T> void prePersist(T entity, RepositoryID context) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
		Objects.requireNonNull(context, ErrorUtils.constructNPXMessage("context"));
		ensureCorrectRepositoryId(context);
		startTransactionIfNotActive();
	}

	/**
	 * Preliminary steps before running {@link #remove(Object, RepositoryID)}
	 * </p>
	 * 
	 * Checks for module state, validates arguments and starts a transaction if
	 * necessary.
	 */
	protected void preRemove(Object primaryKey, RepositoryID context) throws OntoDriverException {
		ensureOpen();
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(context, ErrorUtils.constructNPXMessage("context"));
		ensureCorrectRepositoryId(context);
		startTransactionIfNotActive();
	}

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
	 * Ensures that the specified identifier represents the same repository as
	 * is managed by this storage module.
	 * 
	 * @param repo
	 *            Repository identifier
	 * @throws IllegalArgumentException
	 *             If the validation fails
	 */
	protected void ensureCorrectRepositoryId(RepositoryID repo) {
		assert repo != null;
		if (!repository.getId().equals(repo.getRepository())) {
			throw new IllegalArgumentException("The specified repository identifier " + repo
					+ " does not match the repository " + repository
					+ ", managed by this storage module.");
		}
	}

	/**
	 * Retrieves a new primary key number and increments the internal counter.
	 * 
	 * @param ctx
	 *            The context from which key should be retrieved
	 * @return primary key number
	 */
	protected static int getNewPrimaryKey(Repository repo) {
		assert primaryKeyCounters.containsKey(repo.getId());
		return primaryKeyCounters.get(repo.getId()).incrementAndGet();
	}

	/**
	 * Increments the primary key counter in the specified context.
	 * 
	 * @param ctx
	 *            Context
	 */
	protected static void incrementPrimaryKeyCounter(Repository repo) {
		assert primaryKeyCounters.containsKey(repo.getId());
		primaryKeyCounters.get(repo.getId()).incrementAndGet();
	}
}
