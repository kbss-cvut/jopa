package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.List;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

interface ModuleInternal {

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
	public <T> T readEntity(Class<T> cls, Object primaryKey) throws OntoDriverException;

	/**
	 * Persists the specified entity. </p>
	 * 
	 * @param entity
	 *            The entity to persist
	 * @param primaryKey
	 *            Primary key of the entity. Optional, if the primary key is not
	 *            specified it will be generated and set on the entity
	 * @throws OntoDriverException
	 *             If an error occurs during persist
	 * @throws NullPointerException
	 *             If {@code entity} is null
	 */
	public <T> void persistEntity(T entity, Object primaryKey) throws OntoDriverException;

	/**
	 * Merges state of the specified entity into this module. </p>
	 * 
	 * @param entity
	 *            The entity to merge
	 * @param primaryKey
	 *            Primary key
	 * @throws OntoDriverException
	 *             If the entity is not persistent or if an error occurs during
	 *             merge
	 * @throws NullPointerException
	 *             If {@code entity} or {@code primaryKey} is null
	 */
	public <T> void mergeEntity(T entity, Object primaryKey) throws OntoDriverException;

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
	 * @param fieldName
	 *            Name of the field
	 * @throws OntoDriverException
	 *             If the entity has no field with name {@code fieldName} or if
	 *             an error occurs during load
	 * @throws NullPointerException
	 *             If {@code entity} or {@code fieldName} is null
	 */
	public <T> void loadFieldValue(T entity, String fieldName) throws OntoDriverException;

	/**
	 * Retrieves changes performed since the last {@code commit} and resets the
	 * change list. </p>
	 * 
	 * All changes that were applied since the last {@code commit} are returned
	 * and the list that tracks changes in this ModuleInternal is reset.
	 * 
	 * @return List of changes applied since last call of this method
	 */
	public List<OwlOntologyChangeWrapper> commitAndRetrieveChanges();
}
