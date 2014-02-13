package cz.cvut.kbss.ontodriver.impl;

import java.lang.reflect.Field;
import java.util.List;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

/**
 * Internal storage module implementation. </p>
 * 
 * This interface defines the basic methods which are performed by the storage
 * modules. </p>
 * 
 * The implementations perform the actual mapping between entity model and the
 * ontology.
 * 
 * @author ledvima1
 * 
 * @param <X>
 *            Type of changes returned on commit. Depends on the ontology
 *            manipulation framework (e. g. OWLAPI, Sesame, Jena)
 * @param <Y>
 *            SPARQL statement implementation
 */
public interface ModuleInternal<X, Y> {

	/**
	 * Resolves whether this module contains entity with the specified primary
	 * key. </p>
	 * 
	 * @param primaryKey
	 *            Primary key
	 * @return {@code true} if there is entity with the specified primary key in
	 *         this internal, {@code false} otherwise
	 * @throws OntoDriverException
	 *             If {@code primaryKey} is not a valid URI
	 * @throws NullPointerException
	 *             If {@code primaryKey} is {@code null}
	 */
	public boolean containsEntity(Object primaryKey) throws OntoDriverException;

	/**
	 * Retrieves entity with the specified primary key from this module. </p>
	 * 
	 * @param cls
	 *            Entity class to which the returned object should be cast
	 * @param primaryKey
	 *            Primary key
	 * @return The object with specified primary key or null
	 * @throws OntoDriverException
	 *             If an error occurs during load
	 * @throws NullPointerException
	 *             If {@code cls} or {@code primaryKey} is null
	 */
	public <T> T findEntity(Class<T> cls, Object primaryKey) throws OntoDriverException;

	/**
	 * Checks whether the underlying ontology is consistent.
	 * 
	 * @return {@code true} if the context is consistent, {@code false}
	 *         otherwise
	 * @throws OntoDriverException
	 *             If an error occurs during consistency check
	 */
	public boolean isConsistent() throws OntoDriverException;

	/**
	 * Persists the specified entity. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the entity. Optional, if the primary key is not
	 *            specified it will be generated and set on the entity
	 * @param entity
	 *            The entity to persist
	 * 
	 * @throws OntoDriverException
	 *             If an error occurs during persist
	 * @throws NullPointerException
	 *             If {@code entity} is null
	 */
	public <T> void persistEntity(Object primaryKey, T entity) throws OntoDriverException;

	/**
	 * Merges state of the specified entity field into this module. </p>
	 * 
	 * @param primaryKey
	 *            Primary key
	 * @param entity
	 *            The entity to merge
	 * @param mergedField
	 *            The field to merge
	 * @throws OntoDriverException
	 *             If the entity is not persistent or if an error occurs during
	 *             merge
	 * @throws NullPointerException
	 *             If {@code entity} or {@code primaryKey} is null
	 */
	public <T> void mergeEntity(Object primaryKey, T entity, Field mergedField)
			throws OntoDriverException;

	/**
	 * Removes entity with the specified primary key. </p>
	 * 
	 * @param primaryKey
	 *            Primary key of the entity to remove
	 * @throws OntoDriverException
	 *             If no entity with {@code primaryKey} exists or if an error
	 *             occurs during removal
	 * @throws NullPointerException
	 *             If {@code primaryKey} is null
	 */
	public void removeEntity(Object primaryKey) throws OntoDriverException;

	/**
	 * Loads value of field {@code fieldName} to the entity. </p>
	 * 
	 * This method is intended to be used to load lazily loaded references.
	 * 
	 * @param entity
	 *            The entity
	 * @param field
	 *            The field to load
	 * @throws OntoDriverException
	 *             If the entity has no field with name {@code fieldName} or if
	 *             an error occurs during load
	 * @throws NullPointerException
	 *             If {@code entity} or {@code fieldName} is null
	 */
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException;

	/**
	 * Rolls back all pending changes.
	 */
	public void rollback();

	/**
	 * Resets this internal, causing the working ontology to reload.
	 * 
	 * @throws OntoDriverException
	 *             If an error occurs during internal reset. Typically if the
	 *             internal is unable to reload data from ontology
	 */
	public void reset() throws OntoDriverException;

	/**
	 * Executes the specified SPARQL statement.
	 * 
	 * @param statement
	 *            The statement to execute
	 * @return Result set with statement results
	 */
	public ResultSet executeStatement(Y statement);

	/**
	 * Retrieves changes performed since the last {@code commit} and resets the
	 * change list. </p>
	 * 
	 * All changes that were applied since the last {@code commit} are returned
	 * and the list that tracks changes in this ModuleInternal is reset.
	 * 
	 * @return List of changes applied since last call of this method
	 */
	public List<X> commitAndRetrieveChanges();
}
