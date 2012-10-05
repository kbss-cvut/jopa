package cz.cvut.kbss.jopa.sessions;

import java.util.Map;

/**
 * This interface defines basic methods for accessing the shared live object
 * cache.
 * 
 * @author kidney
 * 
 */
public interface CacheManager extends Cache {

	/**
	 * Add the specified object into the shared session cache. This method gets
	 * the IRI of the object in parameter, so it does not need to extract it
	 * itself.
	 * 
	 * @param primaryKey
	 *            Primary key of the specified object
	 * @param entity
	 *            The object to be added into the cache
	 */
	public void add(Object primaryKey, Object entity);

	/**
	 * Add all of these objects into the shared session cache.
	 * 
	 * The map is expected to contain pairs of primary key and entity with that
	 * primary key (where the primary key is also the key in the map).
	 * 
	 * @param entities
	 *            Map of entities to add to the cache
	 */
	public void addAll(Map<?, ?> entities);

	/**
	 * Get entity with the specified primary key from the cache.
	 * 
	 * @param cls
	 *            Class of the entity
	 * @param primaryKey
	 *            Primary key of the entity to search for
	 * 
	 * @return Object with the specified primary key or null
	 */
	public Object get(Class<?> cls, Object primaryKey);

	/**
	 * Remove objects with inferred attributes from the cache, since there are
	 * changes in the ontology that might influence the inferred attributes.
	 */
	public void clearInferredObjects();
}
