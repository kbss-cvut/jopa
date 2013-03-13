package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
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

	private static final Logger LOG = Logger.getLogger(CacheManagerImpl.class.getName());

	private static final Long DEFAULT_TTL = 60000L;
	// Initial delay is the sweep rate multiplied by this multiplier
	private static final int DELAY_MULTIPLIER = 2;
	private static final long DEFAULT_SWEEP_RATE = 30L;

	private Set<Class<?>> inferredClasses;

	private final Map<URI, Map<Class<?>, Map<Object, Object>>> objCache;
	private final Map<URI, Map<Class<?>, Map<Object, Long>>> ttls;

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

	public CacheManagerImpl(AbstractSession session, Map<String, String> properties) {
		this.session = session;
		this.objCache = new HashMap<URI, Map<Class<?>, Map<Object, Object>>>();
		this.ttls = new HashMap<URI, Map<Class<?>, Map<Object, Long>>>();
		initSettings(properties);
		this.lock = new ReentrantReadWriteLock();
		this.readLock = lock.readLock();
		this.writeLock = lock.writeLock();
		this.cacheSweeper = new CacheSweeper();
		this.sweeperScheduler = Executors.newSingleThreadScheduledExecutor();
		sweeperScheduler.scheduleAtFixedRate(cacheSweeper, initDelay, sweepRate, TimeUnit.SECONDS);
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
				throw new OWLPersistenceException("Unable to parse long value from "
						+ properties.get(OWLAPIPersistenceProperties.CACHE_TTL), e);
			}
		}
		if (!properties.containsKey(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE)) {
			this.sweepRate = DEFAULT_SWEEP_RATE;
		} else {
			try {
				this.sweepRate = Long.parseLong(properties
						.get(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE));
			} catch (NumberFormatException e) {
				throw new OWLPersistenceException("Unable to parse long value from "
						+ properties.get(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE), e);
			}
		}
		this.initDelay = DELAY_MULTIPLIER * sweepRate;
	}

	@Override
	public void add(URI contextUri, Object primaryKey, Object entity) {
		if (primaryKey == null || entity == null || contextUri == null) {
			throw new NullPointerException("Null passed to add: primaryKey = " + primaryKey
					+ ", entity = " + entity + ", contextUri = " + contextUri);
		}
		putObjectIntoCache(primaryKey, entity, contextUri);
	}

	/**
	 * Put the specified object to our cache.
	 * 
	 * @param primaryKey
	 *            Primary key of the cached entity
	 * @param entity
	 *            The entity to cache
	 */
	protected final void putObjectIntoCache(Object primaryKey, Object entity, URI contextUri) {
		assert entity != null;
		assert primaryKey != null;
		assert contextUri != null;
		final Class<?> cls = entity.getClass();
		if (contains(contextUri, cls, primaryKey)) {
			return;
		}
		Map<Class<?>, Map<Object, Object>> m = objCache.get(contextUri);
		if (m == null) {
			m = new HashMap<Class<?>, Map<Object, Object>>();
			objCache.put(contextUri, m);
		}
		Map<Object, Object> entityMap = m.get(cls);
		if (entityMap == null) {
			entityMap = createMap();
			m.put(cls, entityMap);
		}
		Map<Class<?>, Map<Object, Long>> ttlM = ttls.get(contextUri);
		if (ttlM == null) {
			ttlM = new HashMap<Class<?>, Map<Object, Long>>();
			ttls.put(contextUri, ttlM);
		}
		Map<Object, Long> ttlMap = ttlM.get(cls);
		if (ttlMap == null) {
			ttlMap = new HashMap<Object, Long>();
			ttlM.put(cls, ttlMap);

		}
		entityMap.put(primaryKey, entity);
		ttlMap.put(primaryKey, System.currentTimeMillis());
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
	 * Releases the live object cache.
	 */
	private void releaseCache() {
		objCache.clear();
		ttls.clear();
	}

	public void clearInferredObjects() {
		for (Class<?> c : getInferredClasses()) {
			evict(c);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public <T> T get(Class<T> cls, Object primaryKey) {
		if (cls == null || primaryKey == null) {
			return null;
		}
		for (Entry<URI, Map<Class<?>, Map<Object, Object>>> e : objCache.entrySet()) {
			final Map<Class<?>, Map<Object, Object>> m = e.getValue();
			final Object entity = getMapForClass(m, cls).get(primaryKey);
			if (entity == null) {
				continue;
			}
			updateEntityTimeToLive(e.getKey(), cls, primaryKey);
			// We can assume that the entity can be cast to cls (based on
			// insertion
			// logic)
			return cls.cast(entity);
		}
		return null;
	}

	@Override
	public <T> T get(URI contextUri, Class<T> cls, Object primaryKey) {
		if (cls == null || primaryKey == null || contextUri == null) {
			return null;
		}
		final Object entity = getMapForClass(getMapForContext(contextUri), cls).get(primaryKey);
		if (entity == null) {
			return null;
		}
		updateEntityTimeToLive(contextUri, cls, primaryKey);
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
		for (Map<Class<?>, Map<Object, Object>> m : objCache.values()) {
			final Map<Object, Object> map = getMapForClass(m, cls);
			if (map.containsKey(primaryKey)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean contains(URI contextUri, Class<?> cls, Object primaryKey) {
		if (contextUri == null || cls == null || primaryKey == null) {
			return false;
		}
		return getMapForClass(getMapForContext(contextUri), cls).containsKey(primaryKey);
	}

	@Override
	public void evict(URI contextUri, Class<?> cls, Object primaryKey) {
		if (contextUri == null || cls == null || primaryKey == null) {
			throw new NullPointerException("Null passed to evict: primaryKey = " + primaryKey
					+ ", cls = " + cls + ", contextUri = " + contextUri);
		}
		Object e = getMapForClass(getMapForContext(contextUri), cls).remove(primaryKey);
		if (e != null) {
			getTtlMapForClass(getTtlMapForContext(contextUri), cls).remove(primaryKey);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public void evict(Class<?> cls, Object primaryKey) {
		if (cls == null || primaryKey == null) {
			throw new NullPointerException("Null passed to evict: primaryKey = " + primaryKey
					+ ", cls = " + cls);
		}
		for (Map<Class<?>, Map<Object, Object>> m : objCache.values()) {
			final Map<Object, Object> map = getMapForClass(m, cls);
			map.remove(primaryKey);
		}
		for (Map<Class<?>, Map<Object, Long>> m : ttls.values()) {
			final Map<Object, Long> map = getTtlMapForClass(m, cls);
			map.remove(primaryKey);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public void evict(Class<?> cls) {
		if (cls == null) {
			throw new NullPointerException("Null passed to evict: cls = " + cls);
		}
		for (Map<Class<?>, Map<Object, Object>> m : objCache.values()) {
			m.remove(cls);
			// Evict also subclasses
			for (Class<?> c : m.keySet()) {
				if (cls.isAssignableFrom(c)) {
					m.remove(c);
				}
			}
		}
		for (Map<Class<?>, Map<Object, Long>> m : ttls.values()) {
			final Map<Object, Long> ttlMap = getTtlMapForClass(m, cls);
			ttlMap.clear();
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public void evictAll() {
		releaseCache();
	}

	@Override
	public void evict(URI contextUri) {
		if (contextUri == null) {
			throw new NullPointerException("Null passed to evict: contextUri = " + contextUri);
		}
		objCache.remove(contextUri);
		ttls.get(contextUri).clear();
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

	private Map<Class<?>, Map<Object, Object>> getMapForContext(URI context) {
		assert context != null;
		Map<Class<?>, Map<Object, Object>> m = objCache.get(context);
		if (m == null) {
			m = Collections.emptyMap();
		}
		return m;
	}

	/**
	 * Get the map of primary keys and entities of the specified class.
	 * 
	 * @param cls
	 *            Class
	 * @return Map of pairs primary key - entity. If the specified class is not
	 *         registered in the cache, an empty map is returned.
	 */
	private Map<Object, Object> getMapForClass(Map<Class<?>, Map<Object, Object>> context,
			Class<?> cls) {
		assert cls != null;
		Map<Object, Object> m = context.get(cls);
		if (m == null) {
			m = Collections.emptyMap();
		}
		return m;
	}

	private Map<Class<?>, Map<Object, Long>> getTtlMapForContext(URI contextUri) {
		assert contextUri != null;
		Map<Class<?>, Map<Object, Long>> m = ttls.get(contextUri);
		if (m == null) {
			m = Collections.emptyMap();
		}
		return m;
	}

	private Map<Object, Long> getTtlMapForClass(Map<Class<?>, Map<Object, Long>> context,
			Class<?> cls) {
		assert cls != null;
		Map<Object, Long> m = context.get(cls);
		if (m == null) {
			m = Collections.emptyMap();
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
	private void updateEntityTimeToLive(URI contextUri, Class<?> cls, Object primaryKey) {
		assert cls != null;
		assert primaryKey != null;
		if (!ttls.containsKey(contextUri)) {
			return;
		}
		final Map<Class<?>, Map<Object, Long>> ttlMap = getTtlMapForContext(contextUri);
		// We know that there is a cls key in the ttlMap
		ttlMap.get(cls).put(primaryKey, System.currentTimeMillis());
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
				if (CacheManagerImpl.this.sweepRunning || CacheManagerImpl.this.objCache.isEmpty()) {
					return;
				}
				CacheManagerImpl.this.sweepRunning = true;
				final long currentTime = System.currentTimeMillis();
				final List<RecordToEvict> toEvict = new LinkedList<RecordToEvict>();
				// Mark the objects to evict (can't evict them now, it would
				// cause ConcurrentModificationException)
				for (Entry<URI, Map<Class<?>, Map<Object, Long>>> ctxTtl : CacheManagerImpl.this.ttls
						.entrySet()) {
					for (Entry<Class<?>, Map<Object, Long>> ttl : ctxTtl.getValue().entrySet()) {
						for (Entry<Object, Long> e : ttl.getValue().entrySet()) {
							if (e.getValue() + CacheManagerImpl.this.timeToLive < currentTime) {
								toEvict.add(new RecordToEvict(ctxTtl.getKey(), ttl.getKey(), e
										.getKey()));
							}
						}
					}
				}
				// Evict them
				for (RecordToEvict r : toEvict) {
					CacheManagerImpl.this.evict(r.context, r.cls, r.primaryKey);
				}
			} finally {
				CacheManagerImpl.this.sweepRunning = false;
				CacheManagerImpl.this.releaseWriteLock();
			}
		}

	}

	private static final class RecordToEvict {
		private final URI context;
		private final Class<?> cls;
		private final Object primaryKey;

		RecordToEvict(URI context, Class<?> cls, Object primaryKey) {
			this.context = context;
			this.cls = cls;
			this.primaryKey = primaryKey;
		}
	}
}
