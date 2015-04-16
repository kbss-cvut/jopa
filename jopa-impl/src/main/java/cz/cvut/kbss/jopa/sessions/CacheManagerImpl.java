package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

import java.net.URI;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Manages the second level cache shared by all persistence contexts. </p>
 * 
 * This implementation of CacheManager uses cache-wide locking, i. e. the whole
 * cache is locked when an entity is being put in it, no matter that only one
 * context is affected by the change.
 * 
 * 
 * @author kidney
 * 
 */
public class CacheManagerImpl implements CacheManager {

	private static final Logger LOG = Logger.getLogger(CacheManagerImpl.class.getName());

	/** Default time to live in millis */
	private static final long DEFAULT_TTL = 60000L;
	// Initial delay is the sweep rate multiplied by this multiplier
	private static final int DELAY_MULTIPLIER = 2;
	/** Default sweep rate in millis */
	private static final long DEFAULT_SWEEP_RATE = 30000L;

	private Set<Class<?>> inferredClasses;

	private CacheImpl cache;

	// Each repository can have its own lock and they could be acquired by this
	// instance itself, no need to pass this burden to callers
	private final Lock readLock;
	private final Lock writeLock;

	private final ScheduledExecutorService sweeperScheduler;
	private Future<?> sweeperFuture;
	private long initDelay;
	private long sweepRate;
	private long timeToLive;
	private volatile boolean sweepRunning;

	public CacheManagerImpl(Map<String, String> properties) {
		this.cache = new CacheImpl();
		initSettings(properties);
		final ReentrantReadWriteLock lock = new ReentrantReadWriteLock();
		this.readLock = lock.readLock();
		this.writeLock = lock.writeLock();
		this.sweeperScheduler = Executors.newSingleThreadScheduledExecutor();
		this.sweeperFuture = sweeperScheduler.scheduleAtFixedRate(new CacheSweeper(), initDelay, sweepRate,
				TimeUnit.MILLISECONDS);
	}

	private void initSettings(Map<String, String> properties) {
		if (!properties.containsKey(OWLAPIPersistenceProperties.CACHE_TTL)) {
			this.timeToLive = DEFAULT_TTL;
		} else {
			final String strCacheTtl = properties.get(OWLAPIPersistenceProperties.CACHE_TTL);
			try {
				// The property is in seconds, we need milliseconds
				this.timeToLive = Long.parseLong(strCacheTtl) * 1000;
			} catch (NumberFormatException e) {
				LOG.log(Level.SEVERE,
						"Unable to parse cache time to live setting value {0}, using default value.",
						strCacheTtl);
				this.timeToLive = DEFAULT_TTL;
			}
		}
		if (!properties.containsKey(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE)) {
			this.sweepRate = DEFAULT_SWEEP_RATE;
		} else {
			final String strSweepRate = properties
					.get(OWLAPIPersistenceProperties.CACHE_SWEEP_RATE);
			try {
				// The property is in seconds, we need milliseconds
				this.sweepRate = Long.parseLong(strSweepRate) * 1000;
			} catch (NumberFormatException e) {
				LOG.log(Level.SEVERE,
						"Unable to parse sweep rate setting value {0}, using default value.",
						strSweepRate);
				this.sweepRate = DEFAULT_SWEEP_RATE;
			}
		}
		this.initDelay = DELAY_MULTIPLIER * sweepRate;
	}

	@Override
	public void close() {
		if (sweeperFuture != null) {
			if (LOG.isLoggable(Level.FINE)) {
				LOG.fine("Stopping cache sweeper.");
			}
			sweeperFuture.cancel(true);
			sweeperScheduler.shutdown();
		}
		this.sweeperFuture = null;
	}

	@Override
	public void add(Object primaryKey, Object entity, URI context) {
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));

		acquireWriteLock();
		try {
			cache.put(primaryKey, entity, context);
		} finally {
			releaseWriteLock();
		}
	}

	/**
	 * Releases the live object cache.
	 */
	private void releaseCache() {
		acquireWriteLock();
		try {
			this.cache = new CacheImpl();
		} finally {
			releaseWriteLock();
		}
	}

	@Override
	public void clearInferredObjects() {
		acquireWriteLock();
		try {
			getInferredClasses().forEach(cache::evict);
		} finally {
			releaseWriteLock();
		}
	}

	@Override
	public <T> T get(Class<T> cls, Object primaryKey, URI context) {
		if (cls == null || primaryKey == null) {
			return null;
		}
		acquireReadLock();
		try {
			return cache.get(cls, primaryKey, context);
		} finally {
			releaseReadLock();
		}
	}

	/**
	 * Get the set of inferred classes.
	 * 
	 * Inferred classes (i. e. classes with inferred attributes) are tracked
	 * separately since they require special behavior.
	 * 
	 * @return Set of inferred classes
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
	@Override
	public void setInferredClasses(Set<Class<?>> inferredClasses) {
		this.inferredClasses = inferredClasses;
	}

	@Override
	public boolean contains(Class<?> cls, Object primaryKey) {
		if (cls == null || primaryKey == null) {
			return false;
		}
		acquireReadLock();
		try {
			return cache.contains(cls, primaryKey);
		} finally {
			releaseReadLock();
		}
	}

	@Override
	public boolean contains(Class<?> cls, Object primaryKey, URI context) {
		if (cls == null || primaryKey == null) {
			return false;
		}
		acquireReadLock();
		try {
			return cache.contains(cls, primaryKey, context);
		} finally {
			releaseReadLock();
		}
	}

	@Override
	public void evict(Class<?> cls) {
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));

		acquireWriteLock();
		try {
			cache.evict(cls);
		} finally {
			releaseWriteLock();
		}
	}

	@Override
	public void evict(Class<?> cls, Object primaryKey, URI context) {
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));

		acquireWriteLock();
		try {
			cache.evict(cls, primaryKey, context);
		} finally {
			releaseWriteLock();
		}

	}

	@Override
	public void evict(URI context) {
		acquireWriteLock();
		try {
			cache.evict(context);
		} finally {
			releaseWriteLock();
		}
	}

	@Override
	public void evictAll() {
		releaseCache();
	}

	private void acquireReadLock() {
		readLock.lock();
	}

	private void releaseReadLock() {
		readLock.unlock();
	}

	private void acquireWriteLock() {
		writeLock.lock();
	}

	private void releaseWriteLock() {
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
					final long lm = e.getValue();
					if (lm + timeToLive < currentTime) {
						toEvict.add(e.getKey());
					}
				}
				// Evict them
				toEvict.forEach(CacheManagerImpl.this::evict);
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

			final Map<Object, Object> m = getMapForClass(context, cls);
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
