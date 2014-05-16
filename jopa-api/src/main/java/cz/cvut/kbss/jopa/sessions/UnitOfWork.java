package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.List;
import java.util.Set;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityDescriptor;

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
	 * Checks whether context specified by {@code context} is consistent. </p>
	 * 
	 * Can be {@code null}, indicating that consistency of the whole repository
	 * should be checked.
	 * 
	 * @param context
	 *            Context URI
	 * @return {@code true} if the context is consistent, {@code false}
	 *         otherwise
	 * @throws OWLPersistenceException
	 *             If an ontology access error occurs
	 */
	public boolean isConsistent(URI context);

	/**
	 * Merges the state of the given entity into the current persistence
	 * context. </p>
	 * 
	 * The {@code descriptor} argument specified the ontology contexts into
	 * which the detached entity and its fields belong and should be merged.
	 * 
	 * @param entity
	 *            entity instance
	 * @param descriptor
	 *            Repository identifier
	 * @return the managed instance that the state was merged to
	 * @throws NullPointerException
	 *             If {@code entity} or {@code repository} is {@code null}
	 */
	public <T> T mergeDetached(T entity, EntityDescriptor descriptor);

	/**
	 * Retrieves object with the specified primary key. </p>
	 * 
	 * The object as well as its fields are looked for in contexts specified by
	 * the descriptor. The result is then cast to the specified type.
	 * 
	 * @param cls
	 *            The type of the returned object
	 * @param primaryKey
	 *            Primary key
	 * @param descriptor
	 *            Entity descriptor
	 * @return The retrieved object or {@code null} if there is no object with
	 *         the specified primary key in the specified repository
	 * @throws NullPointerException
	 *             If {@code cls}, {@code primaryKey} or {@code repository} is
	 *             {@code null}
	 * @throws OWLPersistenceException
	 *             If {@code repository} is not valid or if an error during
	 *             object loading occurs
	 */
	public <T> T readObject(Class<T> cls, Object primaryKey, EntityDescriptor descriptor);

	/**
	 * Register an existing object in this Unit of Work. The passed object comes
	 * usually from the parent session cache. This method creates a working
	 * clone of this object and puts the given object into this Unit of Work
	 * cache.
	 * 
	 * @param object
	 *            Object
	 * @param descriptor
	 *            Identifier of the repository to which the object being
	 *            registered belongs
	 * @return Object Returns clone of the registered object
	 */
	public Object registerExistingObject(Object object, EntityDescriptor descriptor);

	/**
	 * Registers the specified new object in this Unit of Work. </p>
	 * 
	 * The object will be persisted into the context specified by
	 * {@code descriptor}.
	 * 
	 * @param object
	 *            The object to register
	 * @param descriptor
	 *            Identifier of the repository into which the object should be
	 *            persisted
	 * @throws NullPointerException
	 *             If {@code entity} or {@code context} is {@code null}
	 * @throws OWLPersistenceException
	 *             If {@code context} is not a valid context URI or if an error
	 *             during registration occurs
	 */
	public void registerNewObject(Object object, EntityDescriptor descriptor);

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
	 * Release the current unit of work. Calling this method disregards any
	 * changes made to clones.
	 */
	public void release();

	/**
	 * Reverts any changes to the given object.</p>
	 * 
	 * This method modifies the specified object. The object has to be managed
	 * by this persistence context.
	 * 
	 * @param object
	 *            The object to revert
	 */
	public <T> void revertObject(T object);

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
	 * Gets repository contexts available to this session.
	 * 
	 * @return Unmodifiable list of context URIs
	 */
	public List<URI> getContexts();

	/**
	 * Gets a set of all types managed by this persistence context. </p>
	 * 
	 * I. e. get a set of all known entity classes.
	 * 
	 * @return Set of {@code Class}
	 */
	public Set<Class<?>> getManagedTypes();

	/**
	 * Sets the transactional ontology as the one used for SPARQL query
	 * processing.
	 */
	public void setUseTransactionalOntologyForQueryProcessing();

	/**
	 * Returns true if the transactional ontology is set as the one processing
	 * SPARQL queries.
	 * 
	 * @return boolean
	 */
	public boolean useTransactionalOntologyForQueryProcessing();

	/**
	 * Sets the backup (central) ontology as the one used for SPARQL query
	 * processing.
	 */
	public void setUseBackupOntologyForQueryProcessing();

	/**
	 * Returns true if the backup (central) ontology is set as the one
	 * processing SPARQL queries.
	 * 
	 * @return boolean
	 */
	public boolean useBackupOntologyForQueryProcessing();
}
