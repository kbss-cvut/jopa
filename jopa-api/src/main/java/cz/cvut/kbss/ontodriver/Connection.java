package cz.cvut.kbss.ontodriver;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
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
	 * Checks whether the specified context is consistent. </p>
	 * 
	 * The context URI may be null, meaning that consistency of the whole
	 * repository should be checked.
	 * 
	 * @param context
	 *            Context URI
	 * @return {@code true} if the contexts specified by {@code repository} are
	 *         consistent, {@code false} otherwise
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public boolean isConsistent(URI context) throws OntoDriverException;

	/**
	 * Checks whether the repository contains an individual with the specified
	 * primary key. </p>
	 * 
	 * The {@code context} may be {@code code}, indicating that the whole
	 * repository should be searched.
	 * 
	 * @param primaryKey
	 *            Individual primary key
	 * @param context
	 *            Context URI
	 * @return {@code true} if repository contains matching individual,
	 *         {@code false} otherwise.
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public boolean contains(Object primaryKey, URI context) throws OntoDriverException;

	/**
	 * Finds entity with the specified primary key and returns it as the
	 * specified type. </p>
	 * 
	 * The descriptor specifies in which contexts the entity and its field
	 * should be searched for. If a field's context is not specified, the value
	 * is searched for in the same context as the entity.
	 * 
	 * @param cls
	 *            The of the returned instance
	 * @param primaryKey
	 *            Primary key
	 * @param descriptor
	 *            Entity descriptor
	 * @return Entity or null if there is none with the specified primary key
	 * @throws OntoDriverException
	 *             If called on a closed connection, if the entity cannot be
	 *             cast to the specified type or an ontology access error occurs
	 */
	public <T> T find(Class<T> cls, Object primaryKey, Descriptor descriptor)
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
	 * Retrieves a list of all available contexts. </p>
	 * 
	 * @return Unmodifiable list of available contexts
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public List<URI> getContexts() throws OntoDriverException;

	/**
	 * Loads from ontology and sets value of field {@code fieldName}. </p>
	 * 
	 * @param entity
	 *            Entity to set the field value on
	 * @param field
	 *            The field to load
	 * @param descriptor
	 *            Identifier of a repository context from which the field value
	 *            will be loaded
	 * @throws OntoDriverException
	 *             If called on a closed connection or if an ontology access
	 *             error occurs
	 */
	public <T> void loadFieldValue(T entity, Field field, Descriptor descriptor)
			throws OntoDriverException;

	/**
	 * Merges value of the specified field on the specified entity into the
	 * storage. </p>
	 * 
	 * This method is meant only for merging state of existing entities, trying
	 * to {@code merge} a new entity will result in an exception.
	 * 
	 * @param entity
	 *            The entity to merge
	 * @param mergedField
	 *            The field to merge
	 * @param descriptor
	 *            Specifies context into which the field value will be merged
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 * @throws NullPointerException
	 *             If any of the arguments is {@code null}
	 * @throws IllegalArgumentException
	 *             If the specified entity is not persistent in the specified
	 *             repository or if it has no field corresponding to
	 *             {@code mergedField}
	 */
	public <T> void merge(T entity, Field mergedField, Descriptor descriptor)
			throws OntoDriverException;

	/**
	 * Persists the specified entity into the specified repository. </p>
	 * 
	 * The entity and its fields will be saved into contexts specified by the
	 * descriptor. If a field's context is not specified, it is persisted into
	 * the same context as the entity.
	 * 
	 * @param primaryKey
	 *            Primary key of the new entity. Optional, if not set, it will
	 *            be generated
	 * @param entity
	 *            The entity to persist
	 * @param descriptor
	 *            Entity descriptor
	 * @throws OntoDriverException
	 *             If called on a closed connection, if the repository
	 *             identifier is not valid or an ontology access error occurs
	 */
	public <T> void persist(Object primaryKey, T entity, Descriptor descriptor)
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
	 * Removes entity with the specified primary key. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to be removed
	 * @param descriptor
	 *            Identifier of a repository context from which the entity and
	 *            its fields will be removed
	 * @throws OntoDriverException
	 *             If called on a closed connection or if an ontology access
	 *             error occurs
	 */
	public <T> void remove(Object primaryKey, Descriptor descriptor) throws OntoDriverException;

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
}
