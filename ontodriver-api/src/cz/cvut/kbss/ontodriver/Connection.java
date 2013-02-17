package cz.cvut.kbss.ontodriver;

import java.net.URI;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

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
public interface Connection {

	/**
	 * Closes this connection. </p>
	 * 
	 * Closing an already closed connection does nothing. If there is a
	 * transaction running when {@code close} is called, the transaction is
	 * rolled back. However, it is strongly recommended to commit or roll back
	 * transaction explicitly.
	 * 
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 */
	public void close() throws OntoDriverException;

	/**
	 * Commits the current ontology transaction making all pending changes
	 * persistent. </p>
	 * 
	 * This method should not be called when in auto-commit mode.
	 * 
	 * @throws OntoDriverException
	 *             If in auto-commit mode, called on a closed connection or an
	 *             ontology access error occurs
	 */
	public void commit() throws OntoDriverException;

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
	 * Finds entity with the specified primary key and returns it as the
	 * specified type. </p>
	 * 
	 * This methods searches for the entity in the default context of this
	 * connection. If the context is not set or if no entity with the
	 * {@code primaryKey} is found in it, the rest of the available contexts are
	 * searched (ordered by their priority). Attribute values are looked for in
	 * the same context where the entity was found.
	 * 
	 * @param cls
	 *            Type of the returned instance
	 * @param primaryKey
	 *            Primary key
	 * @return Entity or null if there is none with the specified primary key
	 * @throws OntoDriverException
	 *             If called on a closed connection, if the entity cannot be
	 *             cast to the specified type or an ontology access error occurs
	 * @throws MetamodelNotSetException
	 *             If metamodel is not set for this connection
	 * @see #setDefaultContext(URI)
	 * @see #find(Class, Object, URI)
	 * @see #find(Class, Object, URI, Map)
	 */
	public <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException,
			MetamodelNotSetException;

	/**
	 * Finds entity with the specified primary key and returns it as the
	 * specified type. </p>
	 * 
	 * This method searches the specified {@code context} and if no entity is
	 * found, {@code null} is immediately returned and no further search is
	 * conducted. Attribute values are looked for in the same context as the
	 * entity.
	 * 
	 * @param cls
	 *            The of the returned instance
	 * @param primaryKey
	 *            Primary key
	 * @param context
	 *            URI of Context to search in
	 * @return Entity or null if there is none with the specified primary key
	 * @throws OntoDriverException
	 *             If called on a closed connection, if the entity cannot be
	 *             cast to the specified type or an ontology access error occurs
	 * @throws MetamodelNotSetException
	 *             If metamodel is not set for this connection
	 * @see #find(Class, Object)
	 * @see #find(Class, Object, URI, Map)
	 */
	public <T> T find(Class<T> cls, Object primaryKey, URI context) throws OntoDriverException,
			MetamodelNotSetException;

	/**
	 * Finds entity with the specified primary key and returns it as the
	 * specified type. </p>
	 * 
	 * This method searches for the entity in the specified context.
	 * Furthermore, entity attributes values can be search in different
	 * contexts.
	 * 
	 * @param cls
	 *            Type of the returned instance
	 * @param primaryKey
	 *            Primary key
	 * @param entityContext
	 *            URI of the Context which the entity should be looked for in
	 * @param attributeContexts
	 *            URIs of Contexts where attributes values should be looked for
	 * @return Entity or null if there is none with the specified primary key
	 * @throws OntoDriverException
	 *             If called on a closed connection, if the entity cannot be
	 *             cast to the specified type, if any of the contexts is not
	 *             valid or an ontology access error occurs
	 * @throws MetamodelNotSetException
	 *             If metamodel is not set for this connection
	 * @see #find(Class, Object)
	 * @see #find(Class, Object, URI)
	 */
	public <T> T find(Class<T> cls, Object primaryKey, URI entityContext,
			Map<String, URI> attributeContexts) throws OntoDriverException,
			MetamodelNotSetException;

	/**
	 * Retrieves context with the specified URI.
	 * 
	 * @param contextUri
	 *            URI of the context
	 * @return Context or null if there is none with such URI
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public Context getContext(URI contextUri) throws OntoDriverException;

	/**
	 * Retrieves a list of all available contexts. </p>
	 * 
	 * A context in this scenario can be a named graph, an ontology or an
	 * ontology module.
	 * 
	 * @return List of available contexts
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public List<Context> getContexts() throws OntoDriverException;

	/**
	 * Retrieves saving context for the specified entity. </p>
	 * 
	 * If {@code entity} was loaded from an ontology, its loading context is
	 * also its saving context. If {@code entity} is to be persisted, the
	 * default saving context is returned.
	 * 
	 * @param entity
	 *            The entity to look context up for
	 * @return Context
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 * @see #setDefaultContext(String)
	 */
	public Context getSaveContextFor(Object entity) throws OntoDriverException;

	/**
	 * Merges state of the specified entity into the storage. </p>
	 * 
	 * This method is meant only for merging state of existing entities, trying
	 * to {@code merge} a new entity will result in an exception.
	 * 
	 * @param primaryKey
	 *            Primary key of the merged entity
	 * @param entity
	 *            The entity to merge
	 * @throws OntoDriverException
	 *             If called on a closed connection, if the entity is not
	 *             persistent yet or an ontology access error occurs
	 * @throws MetamodelNotSetException
	 *             If metamodel is not set for this connection
	 */
	public <T> void merge(Object primaryKey, T entity) throws OntoDriverException,
			MetamodelNotSetException;

	/**
	 * Persists the specified entity into the default persistence context. </p>
	 * 
	 * The default context can be either set via the
	 * {@link #setDefaultContext(URI)} or it can be the default graph without
	 * name (for RDF stores).
	 * 
	 * @param primaryKey
	 *            Primary key of the new entity
	 * @param entity
	 *            The entity to persist
	 * @throws OntoDriverException
	 *             If called on a closed connection, the primary key is null or
	 *             an ontology access error occurs
	 * @throws MetamodelNotSetException
	 *             If metamodel is not set for this connection
	 */
	public <T> void persist(Object primaryKey, T entity) throws OntoDriverException,
			MetamodelNotSetException;

	/**
	 * Persists the specified entity into the specified context. </p>
	 * 
	 * The entity is saved into this context with all its attribute values.
	 * 
	 * @param primaryKey
	 *            Primary key of the new entity
	 * @param entity
	 *            The entity to persist
	 * @param context
	 *            URI of the context the new entity will be saved to
	 * @throws OntoDriverException
	 *             If called on a closed connection, the primary key is null, if
	 *             the context is not valid or an ontology access error occurs
	 * @throws MetamodelNotSetException
	 *             If metamodel is not set for this connection
	 */
	public <T> void persist(Object primaryKey, T entity, URI context) throws OntoDriverException,
			MetamodelNotSetException;

	/**
	 * Persists the specified entity. </p>
	 * 
	 * The entity is saved into the {@code context}, the attributes are saved
	 * into respective contexts specified by the {@code attributeContexts}
	 * argument. If context is not set for some attribute, it is saved into the
	 * main context of the entity.
	 * 
	 * @param primaryKey
	 *            Primary key of the new entity
	 * @param entity
	 *            The entity to persist
	 * @param context
	 *            URI of the context the entity will be saved to
	 * @param attributeContexts
	 *            Map of attribute names and context URIs which the attribute
	 *            values will be saved to
	 * @throws OntoDriverException
	 *             If called on a closed connection, the primary key is null, if
	 *             any of the contexts is not valid or an ontology access error
	 *             occurs
	 * @throws MetamodelNotSetException
	 *             If metamodel is not set for this connection
	 */
	public <T> void persist(Object primaryKey, T entity, URI context,
			Map<String, URI> attributeContexts) throws OntoDriverException,
			MetamodelNotSetException;

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
	 * If an entity with the {@code primaryKey} is already loaded in some
	 * context in this connection, the removal is done to this entity. If no
	 * entity is loaded with the {@code primaryKey}, then available contexts are
	 * search (ordered by their priority) and first entity with matching primary
	 * key is removed. </p>
	 * 
	 * This strategy is based on the fact that no assumption can be made about
	 * primary key uniqueness.
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to be removed
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public void remove(Object primaryKey) throws OntoDriverException;

	/**
	 * Removes entity with the specified primary key from the specified context.
	 * </p>
	 * 
	 * If no entity with the specified primary key is found in the specified
	 * context, this method returns and does not try to search other contexts.
	 * </p>
	 * 
	 * If the {@code context} is {@code null}, this method behaves exactly as
	 * {@link #remove(Object)}.
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to be removed
	 * @param context
	 *            URI of the context the entity will be removed from
	 * @throws OntoDriverException
	 *             If called on a closed connection, if the context is not valid
	 *             or if an ontology access error occurs
	 */
	public void remove(Object primaryKey, URI context) throws OntoDriverException;

	/**
	 * Rolls back the current transaction undoing any pending changes. </p>
	 * 
	 * This method should not be called when in auto-commit mode.
	 * 
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public void rollback() throws OntoDriverException;

	/**
	 * Sets auto commit mode on this connection. </p>
	 * 
	 * Setting auto commit twice on to the same value has no effect.
	 * 
	 * @param autoCommit
	 *            True if setting to auto-commit mode, false otherwise
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public void setAutoCommit(boolean autoCommit) throws OntoDriverException;

	/**
	 * Sets default saving context for this connection. </p>
	 * 
	 * This context is used for newly persisted entities without saving context.
	 * 
	 * @param context
	 *            URI of the context to use as default for persist
	 * @throws OntoDriverException
	 *             If the context is not valid, called on a closed connection or
	 *             an ontology access error occurs
	 */
	public void setDefaultContext(URI context) throws OntoDriverException;

	/**
	 * Sets metamodel for this connection. </p>
	 * 
	 * The metamodel is essential for operations that return or require working
	 * with typed entities, since the driver needs to work with the entity's
	 * ontology attributes, e. g. data properties, references.
	 * 
	 * @param metamodel
	 *            {@code Metamodel}
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public void setMetamodel(Metamodel metamodel) throws OntoDriverException;

	/**
	 * Sets saving context for the specified entity. </p>
	 * 
	 * This method is expected to be called mainly for new entities that are yet
	 * to be persisted. However, setting different saving context for an
	 * existing entity is also possible.
	 * 
	 * @param entity
	 *            The entity to set context for
	 * @param context
	 *            The context URI
	 * @throws OntoDriverException
	 *             If called on a closed connection, the context is not valid or
	 *             an ontology access error occurs
	 */
	public void setSaveContextFor(Object entity, URI context) throws OntoDriverException;
}
