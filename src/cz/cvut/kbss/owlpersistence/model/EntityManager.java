package cz.cvut.kbss.owlpersistence.model;

import cz.cvut.kbss.owlpersistence.model.metamodel.Metamodel;

public interface EntityManager {

	/**
	 * (taken from javax.persistence.EntityManager)
	 * 
	 * Make an instance managed and persistent.
	 * 
	 * @param entity
	 * @throws EntityExistsException
	 *             if the entity already exists. (The EntityExistsException may
	 *             be thrown when the persist operation is invoked, or the
	 *             EntityExistsException or another PersistenceException may be
	 *             thrown at flush or commit time.)
	 * @throws IllegalArgumentException
	 *             if not an entity
	 * @throws TransactionRequiredException
	 *             if invoked on a container-managed entity manager of type
	 *             PersistenceContextType.TRANSACTION and there is no
	 *             transaction.
	 */
	public void persist(final Object entity);

	/**
	 * (taken from javax.persistence.EntityManager)
	 * 
	 * Merge the state of the given entity into the current persistence context.
	 * 
	 * @param entity
	 * @return the instance that the state was merged to
	 * @throws IllegalArgumentException
	 *             if instance is not an entity or is a removed entity
	 * @throws TransactionRequiredException
	 *             if invoked on a container-managed entity manager of type
	 *             PersistenceContextType.TRANSACTION and there is no
	 *             transaction.
	 */
	public <T> T merge(final T entity);

	/**
	 * Remove the entity instance.
	 * 
	 * @param entity
	 * @throws IllegalArgumentException
	 *             if not an entity or if a detached entity
	 * @throws TransactionRequiredException
	 *             if invoked on a container-managed entity manager of type
	 *             PersistenceContextType.TRANSACTION and there is no
	 *             transaction.
	 */
	public void remove(final Object entity);

	/**
	 * Find by primary key.
	 * 
	 * @param entityClass
	 * @param primaryKey
	 * @return the found entity instance or null if the entity does not exist
	 * @throws IllegalArgumentException
	 *             if the first argument does not denote an entity type or the
	 *             second argument is not a valid type for that entity’s primary
	 *             key
	 */
	public <T> T find(final Class<T> entityClass, final Object primaryKey);

	// TODO JPA 2.0 find with properties

	// TODO JPA 2.0 find with lock mode

	// TODO JPA 2.0 find with lock mode and properties

	// /**
	// * Get an instance, whose state may be lazily fetched. If the requested
	// * instance does not exist in the database, the EntityNotFoundException is
	// * thrown when the instance state is first accessed. (The persistence
	// * provider runtime is permitted to throw the EntityNotFoundException when
	// * getReference is called.) The application should not expect that the
	// * instance state will be available upon detachment, unless it was
	// accessed
	// * by the application while the entity manager was open.
	// *
	// * @param entityClass
	// * @param primaryKey
	// * @return the found entity instance
	// * @throws IllegalArgumentException
	// * if the first argument does not denote an entity type or the
	// * second argument is not a valid type for that entity’s primary
	// * key
	// * @throws EntityNotFoundException
	// * if the entity state cannot be accessed
	// */
	// public <T> T getReference(final Class<T> entityClass,
	// final Object primaryKey);

	/**
	 * Synchronize the persistence context to the underlying database.
	 * 
	 * @throws TransactionRequiredException
	 *             if there is no transaction
	 * @throws PersistenceException
	 *             if the flush fails
	 */
	public void flush();

	// /**
	// * Set the flush mode that applies to all objects contained in the
	// * persistence context.
	// *
	// * @param flushMode
	// */
	// public void setFlushMode(FlushModeType flushMode);

	// TODO JPA 2.0 getFlushMode

	// /**
	// * Set the lock mode for an entity object contained in the persistence
	// * context.
	// *
	// * @param entity
	// * @param lockMode
	// * @throws PersistenceException
	// * if an unsupported lock call is made
	// * @throws IllegalArgumentException
	// * if the instance is not an entity or is a detached entity
	// * @throws TransactionRequiredException
	// * if there is no transaction
	// */
	// public void lock(Object entity, LockModeType lockMode);

	// TODO JPA 2.0 lock with lock mode and properties

	/**
	 * Refresh the state of the instance from the database, overwriting changes
	 * made to the entity, if any.
	 * 
	 * @param entity
	 * @throws IllegalArgumentException
	 *             if not an entity or entity is not managed
	 * @throws TransactionRequiredException
	 *             if invoked on a container-managed entity manager of type
	 *             PersistenceContextType.TRANSACTION and there is no
	 *             transaction.
	 * @throws EntityNotFoundException
	 *             if the entity no longer exists in the database
	 */
	public void refresh(final Object object);

	// TODO JPA 2.0 refresh with lock mode
	// TODO JPA 2.0 refresh with properties
	// TODO JPA 2.0 refresh with lock mode and properties

	/**
	 * Clear the persistence context, causing all managed entities to become
	 * detached. Changes made to entities that have not been flushed to the
	 * database will not be persisted.
	 */
	public void clear();

	/**
	 * Remove the given entity from the persistence context, causing a managed
	 * entity to become detached. Unflushed changes made to the entity if any
	 * (including removal of the entity), will not be synchronized to the
	 * database. Entities which previously referenced the detached entity will
	 * continue to reference it.
	 * 
	 * @param entity
	 * @throws IllegalArgumentException
	 *             if the instance is not an entity
	 */
	public void detach(Object entity);

	/**
	 * Check if the instance belongs to the current persistence context.
	 * 
	 * @param entity
	 * @return
	 * @throws IllegalArgumentException
	 *             if not an entity
	 */
	public boolean contains(Object entity);

	// TODO JPA 2.0 public LockModeType getLockMode(Object entity)
	// TODO JPA 2.0 setProperty
	// TODO JPA 2.0 getProperties

	/**
	 * Create an instance of Query for executing a Java Persistence query
	 * language statement.
	 * 
	 * @param qlString
	 *            a Java Persistence query string
	 * @return the new query instance
	 * @throws IllegalArgumentException
	 *             if query string is not valid
	 */
	public Query createQuery(String qlString);

	// TODO JPA 2.0 TypedQuery<T> createQuery(CriteriaQuery<T> criteriaQuery)
	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass);

	//
	// /**
	// * Create an instance of Query for executing a named query (in the Java
	// * Persistence query language or in native SQL).
	// *
	// * @param name
	// * the name of a query defined in metadata
	// * @return the new query instance
	// * @throws IllegalArgumentException
	// * if a query has not been defined with the given name
	// */
	// public Query createNamedQuery(String name);
	//
	// TODO JPA 2.0 TypedQuery<T> createNamedQuery(String query, Class<T>
	// resultClass)

	/**
	 * Create an instance of Query for executing a native SQL statement, e.g.,
	 * for update or delete.
	 * 
	 * @param sqlString
	 *            a native SQL query string
	 * @return the new query instance
	 */
	public Query createNativeQuery(String sqlString);

	/**
	 * Create an instance of Query for executing a native SQL query.
	 * 
	 * @param sqlString
	 *            a native SQL query string
	 * @param resultClass
	 *            the class of the resulting instance(s)
	 * @return the new query instance
	 */
	public <T> TypedQuery<T> createNativeQuery(String sqlString,
			Class<T> resultClass);

	// /**
	// * Create an instance of Query for executing a native SQL query.
	// *
	// * @param sqlString
	// * a native SQL query string
	// * @param resultSetMapping
	// * the name of the result set mapping
	// * @return the new query instance
	// */
	// public Query createNativeQuery(String sqlString, String
	// resultSetMapping);

	// /**
	// * Indicate to the EntityManager that a JTA transaction is active. This
	// * method should be called on a JTA application managed EntityManager that
	// * was created outside the scope of the active transaction to associate it
	// * with the current JTA transaction.
	// *
	// * @throws TransactionRequiredException
	// * if there is no transaction.
	// */
	// public void joinTransaction();

	public <T> T unwrap(Class<T> cls);

	/**
	 * Return the underlying provider object for the EntityManager, if
	 * available. The result of this method is implementation specific.
	 */
	public Object getDelegate();

	/**
	 * Close an application-managed EntityManager. After the close method has
	 * been invoked, all methods on the EntityManager instance and any Query
	 * objects obtained from it will throw the IllegalStateException except for
	 * getTransaction and isOpen (which will return false). If this method is
	 * called when the EntityManager is associated with an active transaction,
	 * the persistence context remains managed until the transaction completes.
	 * 
	 * @throws IllegalStateException
	 *             if the EntityManager is container-managed.
	 */
	public void close();

	/**
	 * Determine whether the EntityManager is open.
	 * 
	 * @return true until the EntityManager has been closed.
	 */
	public boolean isOpen();

	/**
	 * Return the resource-level transaction object. The EntityTransaction
	 * instance may be used serially to begin and commit multiple transactions.
	 * 
	 * @return EntityTransaction instance
	 * @throws IllegalStateException
	 *             if invoked on a JTA EntityManager.
	 */
	public EntityTransaction getTransaction();

	/**
	 * @since JPA 2.0
	 */
	public EntityManagerFactory getEntityManagerFactory();

	// TODO JPA 2.0 public CriteriaBuilder getCriteriaBuilder();
	public Metamodel getMetamodel();
}
