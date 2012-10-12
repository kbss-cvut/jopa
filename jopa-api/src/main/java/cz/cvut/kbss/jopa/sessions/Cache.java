package cz.cvut.kbss.jopa.sessions;

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
	 * Whether the cache contains data for the given entity.
	 * 
	 * @param cls
	 *            entity class
	 * @param primaryKey
	 *            primary key
	 * @return boolean indicating whether the entity is in the cache
	 */
	public boolean contains(Class<?> cls, Object primaryKey);

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
	 * Remove the data for entities of the specified class (and its subclasses)
	 * from the cache.
	 * 
	 * @param cls
	 *            entity class
	 */
	public void evict(Class<?> cls);

	/**
	 * Clear the cache.
	 */
	public void evictAll();

}
