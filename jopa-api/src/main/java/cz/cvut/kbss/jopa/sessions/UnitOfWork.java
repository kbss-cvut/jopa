package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.ontodriver.Context;

public interface UnitOfWork extends Session {

	/**
	 * Clears this Unit of Work.
	 */
	public void clear();

	/**
	 * Commit changes to the ontology.
	 */
	public void commit();

	/**
	 * Rolls back changes done since last commit.
	 * 
	 * @see #commit()
	 */
	public void rollback();

	/**
	 * Returns true if the specified entity is managed in the current
	 * persistence context. This method is used by the EntityManager's contains
	 * method.
	 * 
	 * @param entity
	 *            Object
	 * @return
	 */
	public boolean contains(Object entity);

	/**
	 * Return parent session of the current Unit of Work.
	 * 
	 * @return Session
	 */
	public Session getParent();

	/**
	 * Is this Unit of Work active?
	 * 
	 * @return boolean
	 */
	public boolean isActive();

	/**
	 * Returns true if this {@code UnitOfWork} represents persistence context of
	 * a currently running transaction.
	 * 
	 * @return True if in an active transaction
	 */
	public boolean isInTransaction();

	/**
	 * Return true if the given entity is managed. This means it is either in
	 * the shared session cache or it is a new object ready for persist.
	 * 
	 * @param entity
	 *            Object
	 * @return boolean
	 */
	public boolean isObjectManaged(Object entity);

	/**
	 * Merge the state of the given entity into the current persistence context.
	 * 
	 * @param entity
	 *            entity instance
	 * @return the managed instance that the state was merged to
	 * @throws NullPointerException
	 *             If {@code entity} is {@code null}
	 */
	public <T> T mergeDetached(T entity);

	/**
	 * Merge the state of the given entity into the current persistence
	 * context.. </p>
	 * 
	 * The {@code contextUri} argument specified the ontology context into which
	 * the detached entity belongs and should be merged.
	 * 
	 * @param entity
	 *            entity instance
	 * @param contextUri
	 *            URI of the target ontology context
	 * @return the managed instance that the state was merged to
	 * @throws NullPointerException
	 *             If {@code entity} or {@code contextUri} is {@code null}
	 */
	public <T> T mergeDetached(T entity, URI contextUri);

	/**
	 * Retrieves object with the specified primary key. </p>
	 * 
	 * The object is cast to the specified type.
	 * 
	 * @param cls
	 *            The type of the returned object
	 * @param primaryKey
	 *            Primary key
	 * @return The retrieved object or {@code null} if there is no object with
	 *         the specified primary key
	 * @throws NullPointerException
	 *             If {@code cls} or {@code primaryKey} is {@code null}
	 */
	public <T> T readObject(Class<T> cls, Object primaryKey);

	/**
	 * Retrieves object with the specified primary key. </p>
	 * 
	 * The object is looked for in context specified by the {@code context} URI.
	 * The result is then cast to the specified type.
	 * 
	 * @param cls
	 *            The type of the returned object
	 * @param primaryKey
	 *            Primary key
	 * @param context
	 *            Context in which to search
	 * @return The retrieved object or {@code null} if there is no object with
	 *         the specified primary key in the specified context
	 * @throws NullPointerException
	 *             If {@code cls}, {@code primaryKey} or {@code context} is
	 *             {@code null}
	 * @throws OWLPersistenceException
	 *             If {@code context} is not a valid context URI or if an error
	 *             during object load occurs
	 */
	public <T> T readObject(Class<T> cls, Object primaryKey, URI context);

	/**
	 * Register objects from the given collection in this {@code UnitOfWork}.
	 * </p>
	 * 
	 * The difference between this method and the
	 * {@link #registerAllObjects(Collection)} is that this method assumes that
	 * the registered objects exist in the ontology. This version is preffered
	 * and should be used, since it performs better.
	 * 
	 * @param objects
	 *            Objects to register
	 * @return Vector of clones of the specified objects
	 */
	public Vector<Object> registerAllExistingObjects(Collection<Object> objects);

	/**
	 * Register objects from the given collection in this Unit of Work. </p>
	 * 
	 * This creates working clones and puts the given objects into this Unit of
	 * Work cache.
	 * 
	 * @param objects
	 *            Collection<Object>
	 * @return Vector<Object> Returns a Vector of clones of given objects.
	 */
	public Vector<Object> registerAllObjects(Collection<Object> objects);

	/**
	 * Register an existing object in this Unit of Work. The passed object comes
	 * usually from the parent session cache. This method creates a working
	 * clone of this object and puts the given object into this Unit of Work
	 * cache.
	 * 
	 * @param object
	 *            Object
	 * @param contextUri
	 *            URI of the ontology context to which the object being
	 *            registered belongs belongs
	 * @return Object Returns clone of the registered object
	 */
	public Object registerExistingObject(Object object, URI contextUri);

	/**
	 * Registers the specified new object in this Unit of Work. </p>
	 * 
	 * These objects are created during a transaction in this Unit of Work. New
	 * objects are put into ServerSession shared cache after commit.
	 * 
	 * @param entity
	 *            The entity to register
	 * @throws NullPointerException
	 *             If {@code entity} is {@code null}
	 */
	public void registerNewObject(Object entity);

	/**
	 * Registers the specified new object in this Unit of Work. </p>
	 * 
	 * The object will be persisted in the context specified by {@code context}
	 * URI.
	 * 
	 * @param object
	 *            The object to register
	 * @param context
	 *            URI of the context into which the object should be persisted
	 * @throws NullPointerException
	 *             If {@code entity} or {@code context} is {@code null}
	 * @throws OWLPersistenceException
	 *             If {@code context} is not a valid context URI or if an error
	 *             during registration occurs
	 */
	public void registerNewObject(Object object, URI context);

	/**
	 * Remove the given object. Calling this method causes the entity to be
	 * removed from the shared cache and a delete query is initiated on the
	 * ontology.
	 * 
	 * @param object
	 *            Object
	 */
	public void removeObject(Object object);

	/**
	 * This method is the general method for registering objects into Unit of
	 * Work cache. It calls either registerNewObject or registerExistingObject
	 * according to state of the given object.
	 * 
	 * @param object
	 *            Object
	 * @return Object Returns clone of the registered object
	 */
	public Object registerObject(Object object);

	/**
	 * Release the current unit of work. Calling this method disregards any
	 * changes made to clones.
	 */
	public void release();

	/**
	 * Reverts any changes to the given object.
	 * 
	 * @param object
	 *            Object
	 * @return Object The reverted object, it is actually the reverted object
	 *         passed in argument.
	 */
	public Object revertObject(Object object);

	/**
	 * This method returns true, if the UnitOfWork should be released after the
	 * commit call. This is done for inferred attributes, which cause the whole
	 * session cache to be invalidated.
	 * 
	 * @return True if the UnitOfWork should be released after commit.
	 */
	public boolean shouldReleaseAfterCommit();

	/**
	 * Writes any uncommitted changes into the ontology. This method may be
	 * useful when flushing entity manager or closing sessions, because we don't
	 * want to let the changes to get lost.
	 * 
	 */
	public void writeUncommittedChanges();

	/**
	 * Gets a set of all types managed by this persistence context. </p>
	 * 
	 * I. e. get a set of all known entity classes.
	 * 
	 * @return Set of {@code Class}
	 */
	public Set<Class<?>> getManagedTypes();

	/**
	 * Gets a list of available contexts. </p>
	 * 
	 * The contexts are ordered in the list by their priority (descending order)
	 * and the returned {@code List} is not modifiable.
	 * 
	 * @return {@code List} of available contexts
	 * @see Context
	 */
	public List<Context> getContexts();
}
