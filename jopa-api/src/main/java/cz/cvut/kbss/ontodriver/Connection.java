package cz.cvut.kbss.ontodriver;

import java.lang.reflect.Field;
import java.util.List;

import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
import cz.cvut.kbss.ontodriver.exceptions.EntityNotRegisteredException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * Represents a connection to the underlying OntoDriver. </p>
 * 
 * A single OntoDriver can manage multiple storages at once. Each of the storage
 * can be of a different type (OWL, RDF) and different profile (OWL 2 RL, OWL 2
 * EL etc.).
 * 
 * @author kidney
 * 
 */
public interface Connection extends Transactional {

	/**
	 * {@inheritDoc} </p>
	 * 
	 * Calling {@code commit} in auto-commit mode results in an
	 * {@code OntoDriverException}.
	 */
	public void commit() throws OntoDriverException;

	/**
	 * {@inheritDoc} </p>
	 * 
	 * Calling {@code rollback} in auto-commit mode results in an
	 * {@code OntoDriverException}.
	 */
	public void rollback() throws OntoDriverException;

	/**
	 * Creates a new SPARQL statement.
	 * 
	 * @return a {@code Statement} instance
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public Statement createStatement() throws OntoDriverException;

	/**
	 * Resolves whether entity with the specified primary key is present in the
	 * specified repository. </p>
	 * 
	 * @param primaryKey
	 *            Primary key
	 * @param repository
	 *            Repository identifier
	 * @return {@code true} if the specified repository contains entity with the
	 *         given id, {@code false} otherwise
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code repository} is null
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public boolean contains(Object primaryKey, RepositoryID repository) throws OntoDriverException;

	/**
	 * Checks whether the specified repository is consistent. </p>
	 * 
	 * @param repository
	 *            Repository identifier
	 * @return {@code true} if the contexts specified by {@code repository} are
	 *         consistent, {@code false} otherwise
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 * @throws NullPointerException
	 *             if {@code repository} is {@code null}
	 */
	public boolean isConsistent(RepositoryID repository) throws OntoDriverException;

	/**
	 * Finds entity with the specified primary key and returns it as the
	 * specified type. </p>
	 * 
	 * This method searches the specified {@code repository} and if no entity is
	 * found, {@code null} is immediately returned and no further search is
	 * conducted.
	 * 
	 * @param cls
	 *            The of the returned instance
	 * @param primaryKey
	 *            Primary key
	 * @param repository
	 *            Repository identifier
	 * @return Entity or null if there is none with the specified primary key
	 * @throws OntoDriverException
	 *             If called on a closed connection, if the entity cannot be
	 *             cast to the specified type or an ontology access error occurs
	 */
	public <T> T find(Class<T> cls, Object primaryKey, RepositoryID repository)
			throws OntoDriverException;

	/**
	 * Retrieves the current auto-commit status of this {@code Connection}.
	 * 
	 * @return {@code true} if auto-commit is enabled, {@code false} otherwise
	 * @throws OntoDriverException
	 *             If called on a closed connection or if an ontology access
	 *             error occurs
	 */
	public boolean getAutoCommit() throws OntoDriverException;

	/**
	 * Retrieves repository with the specified id.
	 * 
	 * @param repositoryId
	 *            Repository id
	 * @return Repsitory or null if there is none with such id
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public Repository getRepository(Integer repositoryId) throws OntoDriverException;

	/**
	 * Retrieves a list of all available repositories. </p>
	 * 
	 * The repositories are sorted in descending order by their priority, i. e.
	 * as they were passed in initialization.
	 * 
	 * @return List of available repositories
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public List<Repository> getRepositories() throws OntoDriverException;

	/**
	 * Retrieves saving repository for the specified entity. </p>
	 * 
	 * If {@code entity} was loaded from an ontology, its loading repository is
	 * also its saving repository. If the entity was not loaded by this
	 * connection, {@code null} is returned.
	 * 
	 * @param entity
	 *            The entity
	 * @return RepositoryID or {@code null}
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public RepositoryID getSaveRepositoryFor(Object entity) throws OntoDriverException;

	/**
	 * Loads from ontology and sets value of field {@code fieldName}. </p>
	 * 
	 * This method is intended to be used for lazy loaded field values.
	 * 
	 * @param entity
	 *            Entity to set the field value on
	 * @param field
	 *            The field to load
	 * @throws OntoDriverException
	 *             If called on a closed connection or if an ontology access
	 *             error occurs
	 * @throws EntityNotRegisteredException
	 *             If {@code entity} is not registered within this connection
	 */
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException,
			EntityNotRegisteredException;

	/**
	 * Merges value of the specified field on the specified entity into the
	 * storage. </p>
	 * 
	 * This method is meant only for merging state of existing entities, trying
	 * to {@code merge} a new entity will result in an exception.
	 * 
	 * @param primaryKey
	 *            Primary key of the merged entity
	 * @param entity
	 *            The entity to merge
	 * @param The
	 *            field to merge
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 * @throws EntityNotRegisteredException
	 *             If {@code entity} is not registered within this connection
	 * @throws
	 */
	public <T> void merge(Object primaryKey, T entity, Field mergedField)
			throws OntoDriverException;

	/**
	 * Persists the specified entity into the specified repository. </p>
	 * 
	 * The entity is saved into the first context declared in {@code repository}
	 * with all its attribute values.
	 * 
	 * @param primaryKey
	 *            Primary key of the new entity. Optional, if not set, it will
	 *            be generated
	 * @param entity
	 *            The entity to persist
	 * @param repository
	 *            Repository identifier
	 * @throws OntoDriverException
	 *             If called on a closed connection, if the repository
	 *             identifier is not valid or an ontology access error occurs
	 */
	public <T> void persist(Object primaryKey, T entity, RepositoryID repository)
			throws OntoDriverException;

	/**
	 * Creates and returns a new prepared SPARQL statement. </p>
	 * 
	 * @param sparql
	 *            The query to prepare
	 * @return {@code PreparedStatement}
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public PreparedStatement prepareStatement(String sparql) throws OntoDriverException;

	/**
	 * Registers the specified {@code entity} as belonging to the specified
	 * {@code repository} within this connection. </p>
	 * 
	 * No check whether this relationship is correct is done. </p>
	 * 
	 * This method can be used e. g. for registering entities returned from the
	 * second level cache so that the ontology does not have to be queried.
	 * 
	 * @param entity
	 *            The entity to register
	 * @param repository
	 *            Repository identifier
	 * @throws OntoDriverException
	 *             If called on a closed connection or if {@code repository} is
	 *             not valid
	 * @throws NullPointerException
	 *             If {@code entity} or {@code repository} is null
	 */
	public <T> void registerWithRepository(T entity, RepositoryID repository)
			throws OntoDriverException;

	/**
	 * Removes the specified {@code entity}. </p>
	 * 
	 * If the entity is not loaded within this connection an exception is
	 * thrown.
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to be removed
	 * @param entity
	 *            The entity to remove
	 * @throws OntoDriverException
	 *             If called on a closed connection or if an ontology access
	 *             error occurs
	 * @throws EntityNotRegisteredException
	 *             If {@code entity} is not registered within this connection
	 */
	public <T> void remove(Object primaryKey, T entity) throws OntoDriverException;

	/**
	 * Sets auto commit mode on this connection. </p>
	 * 
	 * Setting auto commit twice to the same value has no effect. </p>
	 * 
	 * Note that when auto-commit is enabled, it is not possible to explicitly
	 * commit or roll back transactions. Doing so results in an exception.
	 * 
	 * @param autoCommit
	 *            True if setting to auto-commit mode, false otherwise
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public void setAutoCommit(boolean autoCommit) throws OntoDriverException;

	/**
	 * Sets saving repository for the specified entity. </p>
	 * 
	 * This method is expected to be called mainly for new entities that are yet
	 * to be persisted. However, setting different saving repository for an
	 * existing entity is also possible.
	 * 
	 * @param entity
	 *            The entity to set repository for
	 * @param repository
	 *            Repository identifier
	 * @throws OntoDriverException
	 *             If called on a closed connection, the repository is not valid
	 *             or an ontology access error occurs
	 */
	public void setSaveRepositoryFor(Object entity, RepositoryID repository)
			throws OntoDriverException;
}
