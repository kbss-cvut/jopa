package cz.cvut.kbss.owlpersistence;

import java.lang.reflect.Field;

// fake of the entity manager
public interface EntityManager {

	// Clear the persistence context, causing all managed entities to become
	// detached.
	public void clear();

	// Close an application-managed EntityManager.
	public void close();

	// Check if the instance belongs to the current persistence context.
	public boolean contains(Object entity);

	public <T> T find(final Class<T> entityClass, final Object primaryKey);

	/**
	 * Makes an entity instance managed and persistent. After calling persist(e), the call to contains(e) return true.
	 * 
	 */
	public void persist(final Object entity);

	public void flush();

	public void refresh(final Object object);

	public void remove(final Object object);

	// Determine whether the EntityManager is open.
	public boolean isOpen();
	
	void loadReference(final Object o, final Field fa);

	void saveReference(final Object o, final Field fa);

	// Query createNamedQuery(String name)
	// Create an instance of Query for executing a named query (in the Java
	// Persistence query language or in native SQL).
	// Query createNativeQuery(String sqlString)
	// Create an instance of Query for executing a native SQL statement, e.g.,
	// for update or delete.
	// Query createNativeQuery(String sqlString, Class resultClass)
	// Create an instance of Query for executing a native SQL query.
	// Query createNativeQuery(String sqlString, String resultSetMapping)
	// Create an instance of Query for executing a native SQL query.
	// Query createQuery(String qlString)
	// Create an instance of Query for executing a Java Persistence query
	// language statement.
	// Object getDelegate()
	// Return the underlying provider object for the EntityManager, if
	// available.
	// FlushModeType getFlushMode()
	// Get the flush mode that applies to all objects contained in the
	// persistence context.
	// <T> T
	// getReference(Class<T> entityClass, Object primaryKey)
	// Get an instance, whose state may be lazily fetched.
	// EntityTransaction getTransaction()
	// Returns the resource-level transaction object.
	// void joinTransaction()
	// Indicate to the EntityManager that a JTA transaction is active.
	// void lock(Object entity, LockModeType lockMode)
	// Set the lock mode for an entity object contained in the persistence
	// context.
	// <T> T
	// merge(T entity)
	// Merge the state of the given entity into the current persistence context.
	// void setFlushMode(FlushModeType flushMode)
	// Set the flush mode that applies to all objects contained in the
	// persistence context.
}
