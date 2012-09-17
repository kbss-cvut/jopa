package cz.cvut.kbss.jopa.sessions;

import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.accessors.TransactionOntologyAccessor;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.sessions.CacheManager;
import cz.cvut.kbss.jopa.sessions.Session;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

/**
 * This is the implementation of the basic Session operations. Other more
 * specific methods are to be implemented in descendants. Note: EL uses another
 * object to manage the live object cache. So the current implementation may
 * change if it turns out that more sophisticated access to the cache is needed.
 * 
 * @author kidney
 * 
 */
public abstract class AbstractSession implements Session {
	protected static final Logger LOG = Logger.getLogger(AbstractSession.class
			.getName());

	// this is our session cache
	protected CacheManager liveObjectCache;

	public AbstractSession() {
		super();
	}

	public UnitOfWork acquireUnitOfWork() {
		UnitOfWork uow = new UnitOfWorkImpl(this);
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("UnitOfWork acquired.");
		}
		return uow;
	}

	public CacheManager getLiveObjectCache() {
		if (this.liveObjectCache == null) {
			this.liveObjectCache = new CacheManagerImpl(this);
		}
		return this.liveObjectCache;
	}

	public Query<?> createQuery(String qlString, final EntityManager em) {
		if (qlString == null || qlString.equalsIgnoreCase("")) {
			return null;
		}
		return getOntologyAccessor().createQuery(qlString, em);
	}

	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass,
			boolean sparql, final EntityManager em) {
		if (query == null || query.equalsIgnoreCase("") || resultClass == null) {
			return null;
		}
		return getOntologyAccessor()
				.createQuery(query, resultClass, sparql, em);
	}

	public Query<List<String>> createNativeQuery(String sparql,
			final EntityManager em) {
		if (sparql == null || sparql.equalsIgnoreCase("")) {
			return null;
		}
		return getOntologyAccessor().createNativeQuery(sparql, em);
	}

	/**
	 * This method just releases the live object cache. Subclasses are free to
	 * make additional cleanup.
	 */
	public void release() {
		releaseObjectCache();
	}

	/**
	 * Release the current liveObjectCache. This method is called whenever any
	 * attribute of a cached objects changes during a transaction because then
	 * our cache is no more actual.
	 */
	public void releaseObjectCache() {
		getLiveObjectCache().evictAll();
	}

	/**
	 * Get the ontology accessor. Each client session has its own
	 * TransactionOntologyAccessor and this accessor is valid only during a
	 * single transaction.
	 * 
	 * @return Transaction ontology accessor.
	 */
	public abstract TransactionOntologyAccessor getOntologyAccessor();

	/**
	 * Get a set of all classes managed in this persistence unit - i. e. get all
	 * entity classes.
	 * 
	 * @return Set of managed types.
	 */
	public abstract Set<Class<?>> getManagedTypes();

	/**
	 * Get the metamodel. This is part of the internal API.
	 * 
	 * @return
	 */
	abstract Metamodel getMetamodel();

	/**
	 * Register the specified entity as managed in the specified
	 * {@code UnitOfWork}. </p>
	 * 
	 * Registering loaded entities with their owning {@code UnitOfWork} is
	 * highly recommended, since it speeds up persistence context lookup when
	 * entity attributes are modified.
	 * 
	 * @param entity
	 *            The entity to register
	 * @param uow
	 *            Persistence context of the specified entity
	 */
	abstract void registerEntityWithContext(Object entity, UnitOfWorkImpl uow);
}
