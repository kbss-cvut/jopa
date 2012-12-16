package cz.cvut.kbss.jopa.sessions;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.WeakHashMap;

import javax.persistence.EntityTransaction;

import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.accessors.OntologyAccessor;
import cz.cvut.kbss.jopa.accessors.OntologyAccessorFactory;
import cz.cvut.kbss.jopa.accessors.OntologyAccessorImpl;
import cz.cvut.kbss.jopa.accessors.OntologyDataHolder;
import cz.cvut.kbss.jopa.accessors.TransactionOntologyAccessor;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;

/**
 * The ServerSession is the primary interface for accessing the ontology. </p>
 * 
 * It manages an accessor object, which performs the queries. NOTE: In the
 * future there should be a pool of accessors, since we will be dealing with
 * parallel access from many clients.
 * 
 * @author kidney
 * 
 */
public class ServerSession extends AbstractSession {

	private final Metamodel metamodel;
	private final Set<Class<?>> managedClasses;

	private CacheManager liveObjectCache;
	private final OntologyAccessorFactory accessorFactory;
	private OntologyAccessor ontologyAccessor;

	private final Map<EntityTransaction, EntityManager> runningTransactions;
	private final Map<Object, UnitOfWorkImpl> activePersistenceContexts;
	private final Map<UnitOfWorkImpl, Set<Object>> uowsToEntities;

	public ServerSession() {
		super();
		this.managedClasses = Collections.emptySet();
		this.metamodel = null;
		this.ontologyAccessor = null;
		this.accessorFactory = null;
		this.runningTransactions = new WeakHashMap<EntityTransaction, EntityManager>();
		this.activePersistenceContexts = new WeakHashMap<Object, UnitOfWorkImpl>();
		this.uowsToEntities = new WeakHashMap<UnitOfWorkImpl, Set<Object>>();
	}

	public ServerSession(Map<String, String> properties, Metamodel metamodel,
			OntologyAccessorFactory factory) {
		this.metamodel = metamodel;
		this.managedClasses = processTypes(metamodel.getEntities());
		this.accessorFactory = factory;
		this.runningTransactions = new WeakHashMap<EntityTransaction, EntityManager>();
		this.activePersistenceContexts = new WeakHashMap<Object, UnitOfWorkImpl>();
		this.uowsToEntities = new WeakHashMap<UnitOfWorkImpl, Set<Object>>();
		initialize(properties, metamodel);
	}

	/**
	 * Process the entity types and extract simple Java classes from them.
	 * 
	 * @param entities
	 *            Set of managed entity types.
	 * @return Set of managed entity classes.
	 */
	private Set<Class<?>> processTypes(Set<EntityType<?>> entities) {
		Set<Class<?>> types = new HashSet<Class<?>>(entities.size());
		for (Type<?> t : entities) {
			types.add(t.getJavaType());
		}
		return types;
	}

	/**
	 * Initialize this ServerSession. This in particular means initialization of
	 * the ontology accessor and of the shared cache manager.
	 * 
	 * @param properties
	 *            Map of setup properties.
	 * @param metamodel
	 *            Metamodel of the managed classes and their attributes.
	 * @param factory
	 *            Factory for creating ontology accessors.
	 */
	private void initialize(Map<String, String> properties, Metamodel metamodel) {
		this.ontologyAccessor = accessorFactory.createCentralAccessor(properties, metamodel, this);
		String cache = properties.get(OWLAPIPersistenceProperties.CACHE_PROPERTY);
		if (cache == null || cache.equals("on")) {
			this.liveObjectCache = new CacheManagerImpl(this, properties);
			liveObjectCache.setInferredClasses(metamodel.getInferredClasses());
		} else {
			this.liveObjectCache = new DisabledCacheManager(this);
		}
	}

	/**
	 * Acquire a ClientSession to provide client access to the underlying
	 * resource.
	 * 
	 * @return ClientSession
	 */
	public ClientSession acquireClientSession() {
		ClientSession s = new ClientSession(this);
		return s;
	}

	@Override
	public UnitOfWork acquireUnitOfWork() {
		return acquireClientSession().acquireUnitOfWork();
	}

	public CacheManager getLiveObjectCache() {
		return liveObjectCache;
	}

	public TransactionOntologyAccessor getOntologyAccessor() {
		ontologyAccessor.acquireReadLock();
		final OntologyDataHolder holder = ontologyAccessor.cloneOntologyStructures();
		ontologyAccessor.releaseReadLock();
		return accessorFactory.createTransactionalAccessor(holder, this);
	}

	public OntologyAccessor getAccessor() {
		return this.ontologyAccessor;
	}

	public Map<EntityTransaction, EntityManager> getRunningTransactions() {
		return runningTransactions;
	}

	public boolean transactionStarted(EntityTransaction t, EntityManager em) {
		if (!t.isActive() || t.getRollbackOnly()) {
			return false;
		}
		getRunningTransactions().put(t, em);
		return true;
	}

	public void transactionFinished(EntityTransaction t) {
		if (t == null) {
			return;
		}
		EntityManager em = getRunningTransactions().remove(t);
		if (em == null) {
			return;
		}
		UnitOfWorkImpl uow = (UnitOfWorkImpl) em.getCurrentPersistenceContext();
		if (uow != null && uow.hasChanges()) {
			getLiveObjectCache().clearInferredObjects();
		}
		removePersistenceContext(uow);
	}

	/**
	 * Close the server session and all connections to the underlying data
	 * source.
	 */
	public void close() {
		if (!runningTransactions.isEmpty()) {
			LOG.warning("There are still transactions running. Marking them for rollback.");
			for (EntityTransaction t : getRunningTransactions().keySet()) {
				if (t.isActive()) {
					t.setRollbackOnly();
				}
			}
		}
		ontologyAccessor.close();
	}

	public void releaseClientSession(ClientSession session) {
		// TODO
	}

	public void removeObjectFromCache(Object object) {
		if (object == null) {
			return;
		}
		final IRI primaryKey = ((OntologyAccessorImpl) ontologyAccessor).getIdentifier(object);
		if (primaryKey == null) {
			return;
		}
		getLiveObjectCache().evict(object.getClass(), primaryKey);
	}

	public Vector<?> executeQuery(String sparqlQuery) {
		// TODO Auto-generated method stub
		return null;
	}

	public Vector<?> readAllObjects(Class<?> domainClass) {
		// TODO Auto-generated method stub
		return null;
	}

	public Object readObject(Class<?> domainClass) {
		// TODO Auto-generated method stub
		return null;
	}

	public <T> T readObject(Class<T> cls, Object primaryKey) {
		T result = ontologyAccessor.readEntity(cls, primaryKey);
		getLiveObjectCache().add(primaryKey, result);
		return result;
	}

	public Set<Class<?>> getManagedTypes() {
		return this.managedClasses;
	}

	@Override
	Metamodel getMetamodel() {
		return metamodel;
	}

	public OntologyAccessorFactory getAccessorFactory() {
		return accessorFactory;
	}

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
	void registerEntityWithContext(Object entity, UnitOfWorkImpl uow) {
		if (entity == null || uow == null) {
			throw new NullPointerException("Null passed to as argument. Entity: " + entity
					+ ", unit of work: " + uow);
		}
		activePersistenceContexts.put(entity, uow);
		if (!uowsToEntities.containsKey(uow)) {
			uowsToEntities.put(uow, new HashSet<Object>());
		}
		uowsToEntities.get(uow).add(entity);
	}

	/**
	 * Get persistence context for the specified entity. </p>
	 * 
	 * @param entity
	 *            The entity
	 * @return Persistence context of the specified entity or null, if it cannot
	 *         be found
	 */
	public synchronized UnitOfWorkImpl getPersistenceContext(Object entity) {
		if (entity == null) {
			return null;
		}
		final UnitOfWorkImpl uow = activePersistenceContexts.get(entity);
		return uow;
	}

	/**
	 * Remove the specified {@code UnitOfWork} from the list of currently active
	 * persistence contexts. </p>
	 * 
	 * Also remove all the objects associated with this persistence context.
	 * 
	 * @param uow
	 *            The persistence context to remove
	 */
	private void removePersistenceContext(UnitOfWorkImpl uow) {
		if (uowsToEntities.containsKey(uow)) {
			for (Object entity : uowsToEntities.get(uow)) {
				activePersistenceContexts.remove(entity);
			}
		}
		uowsToEntities.remove(uow);
	}
}
