package cz.cvut.kbss.jopa.sessions;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;

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

	private static final Logger LOG = Logger.getLogger(CacheManagerImpl.class
			.getName());

	private static final Long DEFAULT_TTL = 60000L;
	// Initial delay is the sweep rate multiplied by this multiplier
	private static final int DELAY_MULTIPLIER = 2;
	private static final long DEFAULT_SWEEP_RATE = 30L;

	private Set<Class<?>> inferredClasses;

	private final Map<Class<?>, Map<Object, Object>> objCache;
	private final Map<Class<?>, Map<Object, Long>> ttls;

	protected final AbstractSession session;

	private final ReadWriteLock lock;
	private final Lock readLock;
	private final Lock writeLock;

	private final CacheSweeper cacheSweeper;
	private final ScheduledExecutorService sweeperScheduler;
	private long initDelay;
	private long sweepRate;
	private Long timeToLive;
	private volatile boolean sweepRunning;

	public CacheManagerImpl(AbstractSession session,
			Map<String, String> properties) {
		this.session = session;
		this.objCache = new HashMap<Class<?>, Map<Object, Object>>();
		this.ttls = new HashMap<Class<?>, Map<Object, Long>>();
		initSettings(properties);
		this.lock = new ReentrantReadWriteLock();
		this.readLock = lock.readLock();
		this.writeLock = lock.writeLock();
		this.cacheSweeper = new CacheSweeper();
		this.sweeperScheduler = Executors.newSingleThreadScheduledExecutor();
		sweeperScheduler.scheduleAtFixedRate(cacheSweeper, initDelay,
				sweepRate, TimeUnit.SECONDS);
	}

	private void initSettings(Map<String, String> properties) {
		if (!properties.containsKey(OWLAPIPersistenceProperties.CACHE_TTL)) {
			this.timeToLive = DEFAULT_TTL;
		} else {
			try {
				// The property is in seconds, we need milliseconds
				this.timeToLive = Long.valueOf(properties
						.get(OWLAPIPersistenceProperties.CACHE_TTL)) * 1000;
			} catch (NumberFormatException e) {
				throw new OWLPersistenceException(
						"Unable to parse long value from "
								+ properties
										.get(OWLAPIPersistenceProperties.CACHE_TTL),
						e);
			}
		}
		if (!properties
				.containsKey(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE)) {
			this.sweepRate = DEFAULT_SWEEP_RATE;
		} else {
			try {
				this.sweepRate = Long.parseLong(properties
						.get(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE));
			} catch (NumberFormatException e) {
				throw new OWLPersistenceException(
						"Unable to parse long value from "
								+ properties
										.get(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE),
						e);
			}
		}
		this.initDelay = DELAY_MULTIPLIER * sweepRate;
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
		Map<Object, Long> ttlMap = ttls.get(cls);
		if (ttlMap == null) {
			ttlMap = new HashMap<Object, Long>();
			ttls.put(cls, ttlMap);

		}
		m.put(primaryKey, entity);
		ttlMap.put(primaryKey, System.currentTimeMillis());
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
		ttls.clear();
	}

	public void clearInferredObjects() {
		for (Class<?> c : getInferredClasses()) {
			evict(c);
			getTtlMapForClass(c).clear();
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public <T> T get(Class<T> cls, Object primaryKey) {
		if (cls == null || primaryKey == null) {
			return null;
		}
		final Object entity = getMapForClass(cls).get(primaryKey);
		if (entity == null) {
			return null;
		}
		updateEntityTimeToLive(cls, primaryKey);
		// We can assume that the entity can be cast to cls (based on insertion
		// logic)
		return cls.cast(entity);
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
		final Map<Object, Long> ttlMap = getTtlMapForClass(cls);
		ttlMap.remove(primaryKey);
	}

	/**
	 * {@inheritDoc}
	 */
	public void evict(Class<?> cls) {
		if (cls == null) {
			return;
		}
		final Map<Object, Object> m = getMapForClass(cls);
		// Evict also subclasses
		for (Class<?> c : objCache.keySet()) {
			if (cls.isAssignableFrom(c)) {
				objCache.get(c).clear();
			}
		}
		m.clear();
		final Map<Object, Long> ttlMap = getTtlMapForClass(cls);
		ttlMap.clear();
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

	private Map<Object, Long> getTtlMapForClass(Class<?> cls) {
		assert cls != null;
		Map<Object, Long> m = ttls.get(cls);
		if (m == null) {
			return Collections.emptyMap();
		}
		return m;
	}

	/**
	 * Refresh the time to live for entity with the specified primary key.
	 * 
	 * @param cls
	 *            Class of the entity
	 * @param primaryKey
	 *            Primary key of the entity
	 */
	private void updateEntityTimeToLive(Class<?> cls, Object primaryKey) {
		assert cls != null;
		assert primaryKey != null;
		if (!ttls.containsKey(cls) || !ttls.get(cls).containsKey(primaryKey)) {
			return;
		}
		ttls.get(cls).put(primaryKey, System.currentTimeMillis());
	}

	public boolean acquireReadLock() {
		readLock.lock();
		return true;
	}

	public void releaseReadLock() {
		readLock.unlock();
	}

	public boolean acquireWriteLock() {
		writeLock.lock();
		return true;
	}

	public void releaseWriteLock() {
		writeLock.unlock();
	}

	/**
	 * Sweeps the second level cache and removes entities with no more time to
	 * live.
	 * 
	 * @author kidney
	 * 
	 */
	private final class CacheSweeper implements Runnable {

		public void run() {
			if (LOG.isLoggable(Level.FINE)) {
				LOG.fine("Running cache sweep.");
			}
			CacheManagerImpl.this.acquireWriteLock();
			try {
				if (CacheManagerImpl.this.sweepRunning
						|| CacheManagerImpl.this.objCache.isEmpty()) {
					return;
				}
				CacheManagerImpl.this.sweepRunning = true;
				final long currentTime = System.currentTimeMillis();
				final Map<Object, Class<?>> toEvict = new HashMap<Object, Class<?>>();
				// Mark the objects to evict (can't evict them now, it would
				// cause ConcurrentModificationException)
				for (Entry<Class<?>, Map<Object, Long>> ttl : CacheManagerImpl.this.ttls
						.entrySet()) {
					for (Entry<Object, Long> e : ttl.getValue().entrySet()) {
						if (e.getValue() + CacheManagerImpl.this.timeToLive < currentTime) {
							toEvict.put(e.getKey(), ttl.getKey());
						}
					}
				}
				// Evict them
				for (Entry<Object, Class<?>> e : toEvict.entrySet()) {
					CacheManagerImpl.this.evict(e.getValue(), e.getKey());
				}
			} finally {
				CacheManagerImpl.this.sweepRunning = false;
				CacheManagerImpl.this.releaseWriteLock();
			}
		}

	}
}
