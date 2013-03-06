package cz.cvut.kbss.jopa.sessions;

import java.util.Collection;
import java.util.Set;
import java.util.Vector;

public interface UnitOfWork extends Session {

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
	 * Merge the specified detached entity into the current persistence context.
	 * 
	 * @param entity
	 *            The detached entity.
	 */
	public void mergeDetached(Object entity);

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
	 * @return Object Returns clone of the registered object
	 */
	public Object registerExistingObject(Object object);

	/**
	 * This method takes newly created object and registers it this Unit of Work
	 * cache. These objects are created during a transaction in this Unit of
	 * Work. New objects are put into ServerSession shared cache after commit.
	 * 
	 * @param object
	 *            Object
	 * @return Object Returns clone of the registered object
	 */
	public Object registerNewObject(Object object);

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
	 * Get a set of all types managed by this persistence context. </p>
	 * 
	 * I. e. get a set of all known entity classes.
	 * 
	 * @return Set of {@code Class}
	 */
	public Set<Class<?>> getManagedTypes();
}
