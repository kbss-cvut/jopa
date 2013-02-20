package cz.cvut.kbss.ontodriver;

import java.net.URI;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

/**
 * Manages the whole underlying ontology storage which can consist of several
 * ontologies or modules (OWL modules or RDF named graphs). </p>
 * 
 * The responsibility of a {@code StorageManager} is to provide unified access
 * to both OWL and RDF storages and thus represent a generic facade to multiple
 * types of ontology storages.
 * 
 * @author kidney
 * 
 */
public abstract class StorageManager implements Transactional {

	protected final Metamodel metamodel;
	protected boolean open;

	public StorageManager(Metamodel metamodel) {
		if (metamodel == null) {
			throw new NullPointerException("Metamodel is cannot be null.");
		}
		this.metamodel = metamodel;
		this.open = true;
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	/**
	 * {@inheritDoc} </p>
	 * 
	 * Implementing subclasses can (and should) override this method to close
	 * any open storage connections they maintain.
	 */
	@Override
	public void close() throws OntoDriverException {
		this.open = false;
	}

	/**
	 * Executes the specified SPARQL statement. </p>
	 * 
	 * If the statement is an update, then the number of affected individuals is
	 * returned in the result set.
	 * 
	 * @param statement
	 *            The statement to execute
	 * @return results of the execution
	 * @throws OntoDriverException
	 *             If an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code statement} is {@code null}
	 */
	public abstract ResultSet executeStatement(Statement statement) throws OntoDriverException;

	/**
	 * Finds entity with the specified primary key and returns it as the
	 * specified entity type. </p>
	 * 
	 * The {@code entityContext} represents context where the individual should
	 * be located, {@code attributeContexts} will be searched for the attribute
	 * values. If the {@code entityContext} is not set, the default context is
	 * searched. If {@code attributeContexts} are not set, the same context as
	 * the {@code entityContext} is searched.
	 * 
	 * @param cls
	 *            Return type
	 * @param primaryKey
	 *            Primary key
	 * @param entityContext
	 *            Context where to look for the entity
	 * @param attributeContexts
	 *            Pairs of attribute names and contexts where the appropriate
	 *            value should be looked for. If not specified, use empty map,
	 *            not {@code null}
	 * @return The found entity or null
	 * @throws OntoDriverException
	 *             If any of the contexts is not valid, if the {@code cls} is
	 *             not an entity class or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code cls}, or {@code primaryKey} or attributeContexts is
	 *             {@code null}
	 */
	public abstract <T> T find(Class<T> cls, Object primaryKey, Context entityContext,
			Map<String, Context> attributeContexts) throws OntoDriverException;

	/**
	 * Returns a list of all available contexts this {@code StorageManager} is
	 * managing. </p>
	 * 
	 * The returned list can never be empty since there always has to be at
	 * least the default context of the loaded ontology.
	 * 
	 * @return List of contexts
	 */
	public abstract List<Context> getAvailableContexts();

	/**
	 * Returns a map of available contexts mapped by their URIs. </p>
	 * 
	 * @return Map of contexts mapped by URIs
	 * @see #getAvailableContexts()
	 */
	public abstract Map<URI, Context> getContextsByUris();

	/**
	 * Merges the state of the specified entity into the appropriate ontology.
	 * </p>
	 * 
	 * The {@code entityContext} represents the context into which the entity
	 * should be merged. If the context does not contain such individual, an
	 * exception is thrown. If the {@code entityContext} is not set, the entity
	 * is merged into the default context (which has the highest priority). </p>
	 * 
	 * Attribute values are persisted into the context as specified by the
	 * {@code attributeContexts} parameter. Attribute values without context are
	 * persisted into the same context as the entity.
	 * 
	 * @param primaryKey
	 *            Primary key of the merged entity
	 * @param entity
	 *            The merged entity
	 * @param entityContext
	 *            Context of the entity
	 * @param attributeContexts
	 *            Attribute values' contexts. If not specified, use an empty
	 *            map, not {@code null}
	 * @throws OntoDriverException
	 *             If the entity is not persistent yet, if any of the contexts
	 *             is not valid or if an ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey}, {@code entity} or
	 *             {@code attributeContexts} is {@code null}
	 */
	public abstract <T> void merge(Object primaryKey, T entity, Context entityContext,
			Map<String, Context> attributeContexts) throws OntoDriverException;

	/**
	 * Persists the specified entity. </p>
	 * 
	 * The {@code entity} is persisted into context specified by the
	 * {@code entityContext} parameter. </p>
	 * 
	 * The {@code entity}'s attribute values are persisted to their respective
	 * contexts as specified by the {@code attributeContexts} map. If context
	 * for an attribute is not specified, it is saved into the same context as
	 * the {@code entity}.
	 * 
	 * @param primaryKey
	 *            Primary key of the persisted entity. Optional, if not set it
	 *            will be generated
	 * @param entity
	 *            The entity to persist
	 * @param entityContext
	 *            Context into which the entity will be persisted
	 * @param attributeContexts
	 *            Contexts for attribute values. If not specified, use an empty
	 *            map, not {@code null}
	 * @throws OntoDriverException
	 *             If the primary key is not set, if an entity with the
	 *             specified primary key already exists in the specified
	 *             context, if any of the contexts is not valid or if an
	 *             ontology access error occurs
	 * @throws NullPointerException
	 *             If {@code entity}, {@code entityContext} or
	 *             {@code attributeContexts} is {@code null}
	 */
	public abstract <T> void persist(Object primaryKey, T entity, Context entityContext,
			Map<String, Context> attributeContexts) throws OntoDriverException;

	/**
	 * Removes entity with the specified primary key from the specified context.
	 * </p>
	 * 
	 * If the context does not contain any individual with the specified primary
	 * key an exception is thrown.
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to remove
	 * @param entityContext
	 *            Context from which the entity should be removed
	 * @throws OntoDriverException
	 *             If {@code entityContext} is not valid, if
	 *             {@code entityContext} does not contain any individual with
	 *             the specified primary key or if an ontology access error
	 *             occurs
	 * @throws NullPointerException
	 *             If {@code primaryKey} or {@code entityContext} is null
	 */
	public abstract void remove(Object primaryKey, Context entityContext)
			throws OntoDriverException;
}
