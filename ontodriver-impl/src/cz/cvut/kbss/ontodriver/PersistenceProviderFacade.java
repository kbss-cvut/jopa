package cz.cvut.kbss.ontodriver;

/**
 * Represents the persistence provider.
 * 
 * @author kidney
 * 
 */
public interface PersistenceProviderFacade {

	/**
	 * Retrieves entity with the specified {@code primaryKey} from the second
	 * level cache. </p>
	 * 
	 * This method is likely to return null, since no entity with
	 * {@code primaryKey} may by present in the second level cache or the cache
	 * may be disabled.
	 * 
	 * @param cls
	 *            Type of the entity
	 * @param primaryKey
	 *            Primary key of the entity to retrieve
	 * @return Entity or null
	 */
	public <T> T getEntityFromLiveObjectCache(Class<T> cls, Object primaryKey);
}
