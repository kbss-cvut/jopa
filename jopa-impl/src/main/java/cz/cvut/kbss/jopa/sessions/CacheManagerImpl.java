package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
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
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

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

	// TODO Modify cache manager to handle locking internally on the repo cache
	// level

	private static final Long DEFAULT_TTL = 60000L;
	// Initial delay is the sweep rate multiplied by this multiplier
	private static final int DELAY_MULTIPLIER = 2;
	private static final long DEFAULT_SWEEP_RATE = 30L;

	private Set<Class<?>> inferredClasses;

	private CacheImpl cache;

	// Each repository can have its own lock and they could be acquired by this
	// instance itself, no need to pass this burden to callers
	private final ReadWriteLock lock;
	private final Lock readLock;
	private final Lock writeLock;

	private final CacheSweeper cacheSweeper;
	private final ScheduledExecutorService sweeperScheduler;
	private long initDelay;
	private long sweepRate;
	private Long timeToLive;
	private volatile boolean sweepRunning;

	public CacheManagerImpl(Map<String, String> properties) {
		this.cache = new CacheImpl();
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
	public void add(Object primaryKey, Object entity, URI context) {
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));

		cache.put(primaryKey, entity, context);
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
		this.cache = new CacheImpl();
	}

	public void clearInferredObjects() {
		for (Class<?> c : getInferredClasses()) {
			evict(c);
		}
	}

	@Override
	public <T> T get(Class<T> cls, Object primaryKey, URI context) {
		if (cls == null || primaryKey == null) {
			return null;
		}
		return cache.get(cls, primaryKey, context);
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
			this.inferredClasses = new HashSet<>();
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
		return cache.contains(cls, primaryKey);
	}

	@Override
	public boolean contains(Class<?> cls, Object primaryKey, URI context) {
		if (cls == null || primaryKey == null) {
			return false;
		}
		return cache.contains(cls, primaryKey, context);
	}

	/**
	 * {@inheritDoc}
	 */
	public void evict(Class<?> cls) {
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		cache.evict(cls);
	}

	@Override
	public void evict(Class<?> cls, Object primaryKey, URI context) {
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));

		cache.evict(cls, primaryKey, context);

	}

	@Override
	public void evict(URI context) {
		cache.evict(context);
	}

	/**
	 * {@inheritDoc}
	 */
	public void evictAll() {
		releaseCache();
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
				if (CacheManagerImpl.this.sweepRunning) {
					return;
				}
				CacheManagerImpl.this.sweepRunning = true;
				final long currentTime = System.currentTimeMillis();
				final long timeToLive = CacheManagerImpl.this.timeToLive;
				final List<URI> toEvict = new ArrayList<>();
				// Mark the objects for eviction (can't evict them now, it would
				// cause ConcurrentModificationException)
				for (Entry<URI, Long> e : cache.ttls.entrySet()) {
					final long lm = e.getValue().longValue();
					if (lm + timeToLive < currentTime) {
						toEvict.add(e.getKey());
					}
				}
				// Evict them
				for (URI u : toEvict) {
					CacheManagerImpl.this.evict(u);
				}
			} finally {
				CacheManagerImpl.this.sweepRunning = false;
				CacheManagerImpl.this.releaseWriteLock();
			}
		}
	}

	private static final class CacheImpl {

		private static final String DEFAULT_CONTEXT_BASE = "http://defaultContext";

		private Map<URI, Map<Class<?>, Map<Object, Object>>> repoCache;
		private Map<URI, Long> ttls;
		private final URI defaultContext;

		private CacheImpl() {
			repoCache = new HashMap<>();
			ttls = new HashMap<>();
			this.defaultContext = URI.create(DEFAULT_CONTEXT_BASE + System.currentTimeMillis());
		}

		private void put(Object primaryKey, Object entity, URI context) {
			assert primaryKey != null;
			assert entity != null;

			final Class<?> cls = entity.getClass();
			final URI ctx = context != null ? context : defaultContext;

			Map<Class<?>, Map<Object, Object>> ctxMap;
			if (!repoCache.containsKey(ctx)) {
				ctxMap = new HashMap<>();
				repoCache.put(ctx, ctxMap);
			} else {
				ctxMap = repoCache.get(ctx);
			}
			Map<Object, Object> clsMap;
			if (!ctxMap.containsKey(cls)) {
				clsMap = new HashMap<>();
				ctxMap.put(cls, clsMap);
			} else {
				clsMap = ctxMap.get(cls);
			}
			clsMap.put(primaryKey, entity);
			updateTimeToLive(ctx);
		}

		private <T> T get(Class<T> cls, Object primaryKey, URI context) {
			assert cls != null;
			assert primaryKey != null;

			final URI ctx = context != null ? context : defaultContext;
			final Map<Object, Object> m = getMapForClass(ctx, cls);
			if (m.containsKey(primaryKey)) {
				updateTimeToLive(ctx);
				return cls.cast(m.get(primaryKey));
			}
			return null;
		}

		private boolean contains(Class<?> cls, Object primaryKey) {
			assert cls != null;
			assert primaryKey != null;
			final Map<Object, Object> m = getMapForClass(defaultContext, cls);
			return m.containsKey(primaryKey);
		}

		private boolean contains(Class<?> cls, Object primaryKey, URI context) {
			assert cls != null;
			assert primaryKey != null;
			if (context == null) {
				return contains(cls, primaryKey);
			}

			final URI ctx = context;
			final Map<Object, Object> m = getMapForClass(ctx, cls);
			return m.containsKey(primaryKey);
		}

		private void updateTimeToLive(URI context) {
			assert context != null;

			ttls.put(context, System.currentTimeMillis());
		}

		private void evict(Class<?> cls, Object primaryKey, URI context) {
			assert cls != null;
			assert primaryKey != null;

			final URI ctx = context != null ? context : defaultContext;
			final Map<Object, Object> m = getMapForClass(ctx, cls);
			if (m.containsKey(primaryKey)) {
				m.remove(primaryKey);
			}
		}

		private void evict(URI context) {
			if (context == null) {
				context = defaultContext;
			}
			if (!repoCache.containsKey(context)) {
				return;
			}
			repoCache.get(context).clear();
			ttls.remove(context);
		}

		private void evict(Class<?> cls) {
			for (Entry<URI, Map<Class<?>, Map<Object, Object>>> e : repoCache.entrySet()) {
				final Map<Class<?>, Map<Object, Object>> m = e.getValue();
				m.remove(cls);
				if (m.isEmpty()) {
					ttls.remove(e.getKey());
				}
			}
		}

		private Map<Object, Object> getMapForClass(URI context, Class<?> cls) {
			assert context != null;
			assert cls != null;

			if (!repoCache.containsKey(context)) {
				return Collections.emptyMap();
			}
			final Map<Class<?>, Map<Object, Object>> ctxMap = repoCache.get(context);
			return (ctxMap.containsKey(cls) ? ctxMap.get(cls) : Collections.emptyMap());
		}
	}
}
