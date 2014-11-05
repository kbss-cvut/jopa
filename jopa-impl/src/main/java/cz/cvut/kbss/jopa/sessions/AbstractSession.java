package cz.cvut.kbss.jopa.sessions;

import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

/**
 * This is the implementation of the basic Session operations. Other more
 * specific methods are to be implemented in descendants.
 * 
 * @author kidney
 * 
 */
public abstract class AbstractSession implements Session {
	protected static final Logger LOG = Logger.getLogger(AbstractSession.class.getName());

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
	 * Get the current live object cache. </p>
	 * 
	 * This manager represents the second level cache.
	 * 
	 * @return Second level cache
	 */
	public abstract CacheManager getLiveObjectCache();

	/**
	 * Acquires connection to the underlying ontology storage. </p>
	 * 
	 * @return Connection
	 */
	protected abstract ConnectionWrapper acquireConnection();

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
	abstract void registerEntityWithPersistenceContext(Object entity, UnitOfWorkImpl uow);

	/**
	 * Detaches the specified entity from its persistence context.
	 * 
	 * @param entity
	 *            The entity to deregister
	 * @param uow
	 *            Persistence context to which the entity belonged
	 */
	abstract void deregisterEntityFromPersistenceContext(Object entity, UnitOfWork uow);
}
