package cz.cvut.kbss.jopa.sessions;

import java.net.URI;

/**
 * Interface used to interact with the second-level cache. If a cache is not in
 * use, the methods of this interface have no effect, except for contains, which
 * returns false.
 * 
 * Taken from JPA 2.
 * 
 * @author kidney
 * 
 */
public interface Cache {

	/**
	 * Checks whether the cache contains data for the given entity.
	 * 
	 * @param cls
	 *            entity class
	 * @param primaryKey
	 *            primary key
	 * @return {@code boolean} indicating whether the entity is in the cache
	 */
	public boolean contains(Class<?> cls, Object primaryKey);

	/**
	 * Checks whether the cache contains data for the given entity (in the given
	 * context only).
	 * 
	 * @param contextUri
	 *            URI of context
	 * @param cls
	 *            Entity class
	 * @param primaryKey
	 *            Primary key
	 * @return {@code boolean} indicating whether the entity is in the cache
	 */
	public boolean contains(URI contextUri, Class<?> cls, Object primaryKey);

	/**
	 * Remove the data for the given entity from the cache
	 * 
	 * @param cls
	 *            entity class
	 * @param primaryKey
	 *            primary key
	 */
	public void evict(Class<?> cls, Object primaryKey);

	/**
	 * Removes the data for the given entity from the cache.
	 * 
	 * @param contextUri
	 *            URI of context
	 * @param cls
	 *            Entity class
	 * @param primaryKey
	 *            Primary key
	 */
	public void evict(URI contextUri, Class<?> cls, Object primaryKey);

	/**
	 * Remove the data for entities of the specified class (and its subclasses)
	 * from the cache.
	 * 
	 * @param cls
	 *            entity class
	 */
	public void evict(Class<?> cls);

	/**
	 * Remove the data for entities of the specified context from the cache.
	 * 
	 * @param contextUri
	 *            URI of {@code Context}
	 */
	public void evict(URI contextUri);

	/**
	 * Clear the cache.
	 */
	public void evictAll();

}
