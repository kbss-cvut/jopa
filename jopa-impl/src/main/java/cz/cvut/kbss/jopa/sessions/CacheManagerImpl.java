package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.ArrayList;
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

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
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

	private final List<Map<URI, Map<Class<?>, Map<Object, Object>>>> objCache;
	// The sweeper will work on repository context level
	private final List<Map<URI, Long>> ttls;
	private final List<Repository> repositories;

	protected final ServerSession session;

	private final ReadWriteLock lock;
	private final Lock readLock;
	private final Lock writeLock;

	private final CacheSweeper cacheSweeper;
	private final ScheduledExecutorService sweeperScheduler;
	private long initDelay;
	private long sweepRate;
	private Long timeToLive;
	private volatile boolean sweepRunning;

	public CacheManagerImpl(ServerSession session, int repositories, Map<String, String> properties) {
		this.session = session;
		this.objCache = new ArrayList<>(repositories);
		this.ttls = new ArrayList<>(repositories);
		for (int i = 0; i < repositories; i++) {
			objCache.add(new HashMap<URI, Map<Class<?>, Map<Object, Object>>>());
			ttls.add(new HashMap<URI, Long>());
		}
		this.repositories = session.getRepositories();
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
	public void add(RepositoryID repository, Object primaryKey, Object entity) {
		if (primaryKey == null || entity == null || repository == null) {
			throw new NullPointerException("Null passed to add: primaryKey = " + primaryKey
					+ ", entity = " + entity + ", repository = " + repository);
		}
		if (repository.getContexts().isEmpty()) {
			throw new IllegalArgumentException("The repository has to specify specify context.");
		}
		putObjectIntoCache(primaryKey, entity, repository);
	}

	/**
	 * Put the specified object to our cache.
	 * 
	 * @param primaryKey
	 *            Primary key of the cached entity
	 * @param entity
	 *            The entity to cache
	 */
	private void putObjectIntoCache(Object primaryKey, Object entity, RepositoryID repository) {
		assert entity != null;
		assert primaryKey != null;
		assert repository != null;
		assert !repository.getContexts().isEmpty();
		final Class<?> cls = entity.getClass();
		final URI contextUri = repository.getContexts().iterator().next();
		final Map<URI, Map<Class<?>, Map<Object, Object>>> repo = objCache.get(repository
				.getRepository());
		Map<Class<?>, Map<Object, Object>> m = repo.get(contextUri);
		if (m == null) {
			m = new HashMap<Class<?>, Map<Object, Object>>();
			repo.put(contextUri, m);
		}
		Map<Object, Object> entityMap = m.get(cls);
		if (entityMap == null) {
			entityMap = createMap();
			m.put(cls, entityMap);
		}
		Map<URI, Long> ttlM = ttls.get(repository.getRepository());
		entityMap.put(primaryKey, entity);
		// Just rewrite the last value
		ttlM.put(contextUri, System.currentTimeMillis());
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

	@Override
	public <T> T get(RepositoryID repository, Class<T> cls, Object primaryKey) {
		if (repository == null || cls == null || primaryKey == null) {
			return null;
		}
		final Object entity = getMapForClass(repository, cls).get(primaryKey);
		if (entity != null) {
			updateContextTimeToLive(repository);
		}
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
		for (Map<URI, Map<Class<?>, Map<Object, Object>>> repo : objCache) {
			for (Map<Class<?>, Map<Object, Object>> ctx : repo.values()) {
				if (!ctx.containsKey(cls)) {
					continue;
				}
				if (ctx.get(cls).containsKey(primaryKey)) {
					return true;
				}
			}
		}
		return false;
	}

	@Override
	public boolean contains(RepositoryID repository, Class<?> cls, Object primaryKey) {
		if (repository == null || cls == null || primaryKey == null) {
			return false;
		}
		return getMapForClass(repository, cls).containsKey(primaryKey);
	}

	/**
	 * {@inheritDoc}
	 */
	public void evict(Class<?> cls) {
		if (cls == null) {
			throw new NullPointerException("Null passed to evict: cls = " + cls);
		}
		int length = objCache.size();
		for (int i = 0; i < length; i++) {
			final Map<URI, Map<Class<?>, Map<Object, Object>>> repo = objCache.get(i);
			for (Entry<URI, Map<Class<?>, Map<Object, Object>>> ctx : repo.entrySet()) {
				Map<Class<?>, Map<Object, Object>> m = ctx.getValue();
				if (!m.containsKey(cls)) {
					continue;
				}
				m.remove(cls);
				if (m.isEmpty()) {
					ttls.get(i).remove(ctx.getKey());
				}
			}
		}
	}

	@Override
	public void evict(RepositoryID repository, Class<?> cls, Object primaryKey) {
		if (repository == null || cls == null || primaryKey == null) {
			throw new NullPointerException("Null passed to CacheManager.contains: repository = "
					+ repository + ", cls = " + cls + ", primaryKey = " + primaryKey);
		}
		getMapForClass(repository, cls).remove(primaryKey);

	}

	@Override
	public void evict(RepositoryID repository) {
		if (repository == null) {
			throw new NullPointerException();
		}
		for (URI ctx : repository.getContexts()) {
			objCache.get(repository.getRepository()).get(ctx).clear();
			ttls.get(repository.getRepository()).remove(ctx);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public void evictAll() {
		releaseCache();
	}

	/**
	 * Get the map of primary keys and entities of the specified class.
	 * 
	 * @param cls
	 *            Class
	 * @return Map of pairs primary key - entity. If the specified class is not
	 *         registered in the cache, an empty map is returned.
	 */
	private Map<Object, Object> getMapForClass(RepositoryID repository, Class<?> cls) {
		assert repository != null;
		assert cls != null;
		assert !repository.getContexts().isEmpty();
		final URI contextUri = repository.getContexts().iterator().next();
		Map<Class<?>, Map<Object, Object>> context = objCache.get(repository.getRepository()).get(
				contextUri);
		if (context == null) {
			context = Collections.emptyMap();
		}
		Map<Object, Object> m = context.get(cls);
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
	private void updateContextTimeToLive(RepositoryID repository) {
		assert repository != null;
		assert !repository.getContexts().isEmpty();
		final Map<URI, Long> ttlMap = ttls.get(repository.getRepository());
		final URI contextUri = repository.getContexts().iterator().next();
		ttlMap.put(contextUri, System.currentTimeMillis());
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
				final List<RepositoryID> toEvict = new LinkedList<>();
				// Mark the objects to evict (can't evict them now, it would
				// cause ConcurrentModificationException)
				int length = CacheManagerImpl.this.repositories.size();
				for (int i = 0; i < length; i++) {
					Map<URI, Long> m = CacheManagerImpl.this.ttls.get(i);
					final RepositoryID repo = new RepositoryID(
							CacheManagerImpl.this.repositories.get(i));
					for (Entry<URI, Long> ttl : m.entrySet()) {
						if (ttl.getValue() + CacheManagerImpl.this.timeToLive < currentTime) {
							repo.addContext(ttl.getKey());
						}
					}
					if (!repo.getContexts().isEmpty()) {
						toEvict.add(repo);
					}
				}
				// Evict them
				for (RepositoryID r : toEvict) {
					CacheManagerImpl.this.evict(r);
				}
			} finally {
				CacheManagerImpl.this.sweepRunning = false;
				CacheManagerImpl.this.releaseWriteLock();
			}
		}

	}
}
