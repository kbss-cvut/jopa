package cz.cvut.kbss.jopa.sessions;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * The CacheManager is responsible for managing the live object cache of our
 * session. It controls adding and removing objects from the cache. It supports
 * finding object by id or by a reference to it.
 * 
 * 
 * @author kidney
 * 
 */
public class CacheManagerImpl implements CacheManager {

	// TODO: Implement proper life time for the cache (objects should be cached
	// only for a certain time and
	// inactive objects should be then evicted)
	// And implement proper locking strategy

	private Set<Class<?>> inferredClasses;

	private final Map<Class<?>, Map<Object, Object>> objCache;

	protected final AbstractSession session;

	public CacheManagerImpl(AbstractSession session) {
		this.session = session;
		this.objCache = new HashMap<Class<?>, Map<Object, Object>>();
	}

	/**
	 * {@inheritDoc}
	 */
	public void add(Object primaryKey, Object entity) {
		if (entity == null || primaryKey == null) {
			return;
		}
		this.putObjectIntoCache(primaryKey, entity);
	}

	/**
	 * Put the specified object to our cache.
	 * 
	 * @param primaryKey
	 *            Primary key of the cached entity
	 * @param entity
	 *            The entity to cache
	 */
	protected final void putObjectIntoCache(Object primaryKey, Object entity) {
		assert entity != null;
		assert primaryKey != null;
		final Class<?> cls = entity.getClass();
		if (contains(cls, primaryKey)) {
			return;
		}
		Map<Object, Object> m = objCache.get(cls);
		if (m == null) {
			m = createMap();
			objCache.put(cls, m);
		}
		m.put(primaryKey, entity);

	}

	/**
	 * This method adds the whole map of objects into the liveObjectCache. It
	 * expects the map to contain pairs of Object's IRIs and the Object itself.
	 * 
	 * @param objects
	 *            Map
	 */
	public void addAll(Map<?, ?> objects) {
		if (objects == null) {
			return;
		}
		for (Map.Entry<?, ?> e : objects.entrySet()) {
			final Object primaryKey = e.getKey();
			final Object entity = e.getValue();
			add(primaryKey, entity);
		}
	}

	/**
	 * Create a particular implementation of the Map interface. This method
	 * enables the implementation to be changed according to our needs.
	 * 
	 * @return A new map instance.
	 */
	protected Map<Object, Object> createMap() {
		return new HashMap<Object, Object>();
	}

	/**
	 * INTERNAL method.
	 * 
	 * @return Map
	 */
	public Map<Class<?>, Map<Object, Object>> getLiveObjectCache() {
		return Collections.unmodifiableMap(objCache);
	}

	/**
	 * Releases the live object cache.
	 */
	private void releaseCache() {
		objCache.clear();
	}

	public void clearInferredObjects() {
		for (Class<?> c : getInferredClasses()) {
			evict(c);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public Object get(Class<?> cls, Object primaryKey) {
		if (cls == null || primaryKey == null) {
			return null;
		}
		return getMapForClass(cls).get(primaryKey);
	}

	/**
	 * Get the set of inferred classes.
	 * 
	 * Inferred classes (i. e. classes with inferred attributes) are tracked
	 * separately since they require special behavior.
	 * 
	 * @return
	 */
	public Set<Class<?>> getInferredClasses() {
		if (inferredClasses == null) {
			this.inferredClasses = new HashSet<Class<?>>();
		}
		return inferredClasses;
	}

	/**
	 * Set the inferred classes.
	 * 
	 * For more information about inferred classes see
	 * {@link #getInferredClasses()}.
	 * 
	 * @param inferredClasses
	 *            The set of inferred classes
	 */
	public void setInferredClasses(Set<Class<?>> inferredClasses) {
		this.inferredClasses = inferredClasses;
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean contains(Class<?> cls, Object primaryKey) {
		if (cls == null || primaryKey == null) {
			return false;
		}
		final Map<Object, Object> m = getMapForClass(cls);
		return m.containsKey(primaryKey);
	}

	/**
	 * {@inheritDoc}
	 */
	public void evict(Class<?> cls, Object primaryKey) {
		if (cls == null || primaryKey == null) {
			return;
		}
		final Map<Object, Object> m = getMapForClass(cls);
		m.remove(primaryKey);
	}

	/**
	 * {@inheritDoc}
	 */
	public void evict(Class<?> cls) {
		final Map<Object, Object> m = getMapForClass(cls);
		m.clear();
	}

	/**
	 * {@inheritDoc}
	 */
	public void evictAll() {
		releaseCache();
	}

	/**
	 * Check if the cache is empty.
	 * 
	 * This method is not part of the public API, but can be useful.
	 * 
	 * @return True if the cache is empty
	 */
	public boolean isEmpty() {
		return objCache.isEmpty();
	}

	/**
	 * Get the map of primary keys and entities of the specified class.
	 * 
	 * @param cls
	 *            Class
	 * @return Map of pairs primary key - entity. If the specified class is not
	 *         registered in the cache, an empty map is returned.
	 */
	private Map<Object, Object> getMapForClass(Class<?> cls) {
		assert cls != null;
		Map<Object, Object> m = objCache.get(cls);
		if (m == null) {
			return Collections.emptyMap();
		}
		return m;
	}

}
