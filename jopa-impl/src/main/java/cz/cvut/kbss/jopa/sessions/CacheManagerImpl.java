package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
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
import cz.cvut.kbss.jopa.model.Repository;
import cz.cvut.kbss.jopa.model.RepositoryID;
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

	private static final Long DEFAULT_TTL = 60000L;
	// Initial delay is the sweep rate multiplied by this multiplier
	private static final int DELAY_MULTIPLIER = 2;
	private static final long DEFAULT_SWEEP_RATE = 30L;

	private Set<Class<?>> inferredClasses;

	private final Map<Integer, CacheImpl> repoCaches;
	private final List<Repository> repositories;

	protected final ServerSession session;

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

	public CacheManagerImpl(ServerSession session, Map<String, String> properties) {
		this.session = session;
		this.repoCaches = new HashMap<>();
		this.repositories = session.getRepositories();
		for (Repository r : repositories) {
			repoCaches.put(r.getId(), new CacheImpl());
		}
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
		getRepositoryCache(repository).put(repository, primaryKey, entity);
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
		for (Repository r : repositories) {
			repoCaches.put(r.getId(), new CacheImpl());
		}
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
		return getRepositoryCache(repository).get(repository, cls, primaryKey);
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
		boolean res = false;
		for (Repository r : repositories) {
			final RepositoryID rid = r.createRepositoryID(true);
			res = repoCaches.get(r.getId()).contains(rid, cls, primaryKey);
			if (res) {
				return res;
			}
		}
		return res;
	}

	@Override
	public boolean contains(RepositoryID repository, Class<?> cls, Object primaryKey) {
		if (repository == null || cls == null || primaryKey == null) {
			return false;
		}
		return getRepositoryCache(repository).contains(repository, cls, primaryKey);
	}

	/**
	 * {@inheritDoc}
	 */
	public void evict(Class<?> cls) {
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		for (CacheImpl c : repoCaches.values()) {
			c.evict(cls);
		}
	}

	@Override
	public void evict(RepositoryID repository, Class<?> cls, Object primaryKey) {
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));

		getRepositoryCache(repository).evict(repository, cls, primaryKey);

	}

	@Override
	public void evict(RepositoryID repository) {
		Objects.requireNonNull(repository, ErrorUtils.constructNPXMessage("repository"));

		getRepositoryCache(repository).evict(repository);
	}

	/**
	 * {@inheritDoc}
	 */
	public void evictAll() {
		releaseCache();
	}

	private CacheImpl getRepositoryCache(RepositoryID repository) {
		return repoCaches.get(repository.getRepository());
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
				final List<RepositoryID> toEvict = new LinkedList<>();
				// Mark the objects for eviction (can't evict them now, it would
				// cause ConcurrentModificationException)
				for (Repository r : repositories) {
					final CacheImpl c = repoCaches.get(r.getId());
					final RepositoryID rid = r.createRepositoryID(false);
					for (Entry<URI, Long> e : c.ttls.entrySet()) {
						final long lm = e.getValue().longValue();
						if (lm + timeToLive < currentTime) {
							rid.addContext(e.getKey());
						}
					}
					if (!rid.getContexts().isEmpty()) {
						toEvict.add(rid);
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

	private static final class CacheImpl {
		private Map<URI, Map<Class<?>, Map<Object, Object>>> repoCache;
		private Map<URI, Long> ttls;

		private CacheImpl() {
			repoCache = new HashMap<>();
			ttls = new HashMap<>();
		}

		private void put(RepositoryID context, Object primaryKey, Object entity) {
			assert !context.getContexts().isEmpty();
			assert primaryKey != null;
			assert entity != null;

			final URI ctx = context.getContexts().iterator().next();
			final Class<?> cls = entity.getClass();

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

		private <T> T get(RepositoryID context, Class<T> cls, Object primaryKey) {
			assert !context.getContexts().isEmpty();
			assert cls != null;
			assert primaryKey != null;

			for (URI ctx : context.getContexts()) {
				final Map<Object, Object> m = getMapForClass(ctx, cls);
				if (m.containsKey(primaryKey)) {
					updateTimeToLive(ctx);
					return cls.cast(m.get(primaryKey));
				}
			}
			return null;
		}

		private boolean contains(RepositoryID context, Class<?> cls, Object primaryKey) {
			assert !context.getContexts().isEmpty();
			assert cls != null;
			assert primaryKey != null;

			for (URI ctx : context.getContexts()) {
				final Map<Object, Object> m = getMapForClass(ctx, cls);
				if (m.containsKey(primaryKey)) {
					return true;
				}
			}
			return false;
		}

		private void updateTimeToLive(URI context) {
			assert context != null;

			ttls.put(context, System.currentTimeMillis());
		}

		private void evict(RepositoryID context, Class<?> cls, Object primaryKey) {
			assert !context.getContexts().isEmpty();
			assert cls != null;
			assert primaryKey != null;

			for (URI ctx : context.getContexts()) {
				final Map<Object, Object> m = getMapForClass(ctx, cls);
				if (m.containsKey(primaryKey)) {
					m.remove(primaryKey);
					break;
				}
			}
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

		private void evict(RepositoryID contexts) {
			for (URI ctx : contexts.getContexts()) {
				repoCache.remove(ctx);
				ttls.remove(ctx);
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
