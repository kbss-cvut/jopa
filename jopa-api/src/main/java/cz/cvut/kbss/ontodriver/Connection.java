package cz.cvut.kbss.ontodriver;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.ontodriver.exceptions.MetamodelNotSetException;
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
	 * Resolves whether entity with the specified primary key is present in this
	 * connection's default context. </p>
	 * 
	 * This method also searches ontologies imported by the ontology represented
	 * by the default context.
	 * 
	 * @param primaryKey
	 *            Primary key
	 * @return {@code true} if the default context contains entity with the
	 *         specified primary key, {@code false} otherwise
	 * @throws NullPointerException
	 *             If {@code primaryKey} is null
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 * @see #contains(Object, URI)
	 */
	public boolean contains(Object primaryKey) throws OntoDriverException;

	/**
	 * Resolves whether entity with the specified primary key is present in
	 * context with the specified URI. </p>
	 * 
	 * @param primaryKey
	 *            Primary key
	 * @param context
	 *            URI of Context to search in
	 * @return {@code true} if the default context contains entity with the
	 *         specified primary key, {@code false} otherwise
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code context} is null
	 * @throws OntoDriverException
	 *             If called on a closed connection or an ontology access error
	 *             occurs
	 */
	public boolean contains(Object primaryKey, URI context) throws OntoDriverException;

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
	 * @see #setConnectionContext(URI)
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
	 * Retrieves the current auto-commit status of this {@code Connection}.
	 * 
	 * @return {@code true} if auto-commit is enabled, {@code false} otherwise
	 * @throws OntoDriverException
	 *             If called on a closed connection or if an ontology access
	 *             error occurs
	 */
	public boolean getAutoCommit() throws OntoDriverException;

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
	 * Retrieves the current context of this {@code Connection}. </p>
	 * 
	 * The current context is used when no saving context is specified for e. g.
	 * {@code persist}. </p>
	 * 
	 * By default the context with highest priority is used as current context.
	 * This can be changed by calling {@link #setConnectionContext(URI)}.
	 * 
	 * @return {@code Context} of this connection
	 * @throws OntoDriverException
	 *             If called on a closed connection or if an ontology access
	 *             error occurs
	 * @see #setConnectionContext(URI)
	 */
	public Context getCurrentContext() throws OntoDriverException;

	/**
	 * Retrieves a list of all available contexts. </p>
	 * 
	 * A context in this scenario can be a named graph, an ontology or an
	 * ontology module. </p>
	 * 
	 * The contexts are sorted in descending order by their priority.
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
	 * @see #setConnectionContext(String)
	 */
	public Context getSaveContextFor(Object entity) throws OntoDriverException;

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
	 *             If called on a closed connection, if the entity is not
	 *             persistent or if an ontology access error occurs
	 */
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException;

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
	 * Persists the specified entity into a context. </p>
	 * 
	 * The context can be:
	 * <ul>
	 * <li>set for the entity via the {@link #setSaveContextFor(Object, URI)}</li>
	 * <li>set for the whole connection via {@link #setConnectionContext(URI)}</li>
	 * <li>or initially it is context with the highest priority.</li>
	 * </ul>
	 * 
	 * In this order.
	 * 
	 * @param primaryKey
	 *            Primary key of the new entity. Optional, if not set it will be
	 *            generated
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
	 *            Primary key of the new entity. Optional, if not set it will be
	 *            generated
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
	 *            Primary key of the new entity. Optional, if not set it will be
	 *            generated
	 * @param entity
	 *            The entity to persist
	 * @param entityContext
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
	public <T> void persist(Object primaryKey, T entity, URI entityContext,
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
	 * Registers the specified {@code entity} as belonging to the specified
	 * {@code context} within this connection. </p>
	 * 
	 * No check whether this relationship is correct is done. </p>
	 * 
	 * This method can be used e. g. for registering entities returned from the
	 * second level cache so that the ontology does not have to be queried.
	 * 
	 * @param entity
	 *            The entity to register
	 * @param context
	 *            The context
	 * @throws OntoDriverException
	 *             If called on a closed connection or if {@code context} is not
	 *             valid
	 * @throws NullPointerException
	 *             If {@code entity} or {@code context} is null
	 */
	public <T> void registerWithContext(T entity, URI context) throws OntoDriverException;

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
	 *             If called on a closed connection, if no entity with
	 *             {@code primaryKey} is loaded within this connection or if an
	 *             ontology access error occurs
	 * @throws MetamodelNotSetException
	 *             If metamodel is not set for this connection
	 */
	public <T> void remove(Object primaryKey, T entity) throws OntoDriverException,
			MetamodelNotSetException;

	/**
	 * Removes the specified {@code entity} from the specified {@code context}.
	 * </p>
	 * 
	 * If the entity is not persistent in the specified context an exception is
	 * thrown. </p>
	 * 
	 * If the {@code context} is {@code null}, this method behaves exactly as
	 * {@link #remove(Object)}.
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to be removed
	 * @param entity
	 *            The entity to remove
	 * @param context
	 *            URI of the context the entity will be removed from
	 * @throws OntoDriverException
	 *             If called on a closed connection, if the context is not valid
	 *             or if an ontology access error occurs
	 * @throws MetamodelNotSetException
	 *             If metamodel is not set for this connection
	 */
	public <T> void remove(Object primaryKey, T entity, URI context) throws OntoDriverException,
			MetamodelNotSetException;

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
	public void setConnectionContext(URI context) throws OntoDriverException;

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
