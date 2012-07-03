package cz.cvut.kbss.jopa.sessions;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.persistence.EntityTransaction;

import cz.cvut.kbss.jopa.accessors.OntologyAccessor;
import cz.cvut.kbss.jopa.accessors.OntologyAccessorFactory;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;

/**
 * The ServerSession is the primary interface for accessing the ontology. It
 * manages an accessor object, which performs the queries. NOTE: In the future
 * there should be a pool of accessors, since we will be dealing with parallel
 * access from many clients.
 * 
 * @author kidney
 * 
 */
public class ServerSession extends AbstractSession {

	private static final String CACHE_PROPERTY = "cache";

	private final Metamodel metamodel;
	private final Set<Class<?>> managedClasses;

	protected OntologyAccessor accessor;

	private Map<EntityTransaction, EntityManager> runningTransactions;

	public ServerSession() {
		super();
		this.managedClasses = Collections.emptySet();
		this.metamodel = null;
	}

	public ServerSession(Map<String, String> properties, Metamodel metamodel,
			OntologyAccessorFactory factory) {
		this.metamodel = metamodel;
		this.managedClasses = processTypes(metamodel.getEntities());
		initialize(properties, metamodel, factory);
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
	private void initialize(Map<String, String> properties,
			Metamodel metamodel, OntologyAccessorFactory factory) {
		this.accessor = factory.createOntologyAccessor(properties, metamodel,
				this);
		String cache = properties.get(CACHE_PROPERTY);
		if (cache == null || cache.equals("on")) {
			CacheManagerImpl cm = (CacheManagerImpl) getLiveObjectCache();
			cm.setInferredClasses(metamodel.getInferredClasses());
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

	public OntologyAccessor getOntologyAccessor() {
		return this.accessor;
	}

	public Map<EntityTransaction, EntityManager> getRunningTransactions() {
		if (runningTransactions == null) {
			this.runningTransactions = new HashMap<EntityTransaction, EntityManager>();
		}
		return runningTransactions;
	}

	public boolean transactionStarted(EntityTransaction t, EntityManager em) {
		if (!t.isActive() || t.getRollbackOnly()) {
			return false;
		}
		getRunningTransactions().put(t, em);
		return true;
	}

	public void transactionCommitted(EntityTransaction t) {
		if (t == null) {
			return;
		}
		EntityManager em = getRunningTransactions().get(t);
		if (em == null) {
			return;
		}
		UnitOfWorkImpl uow = (UnitOfWorkImpl) em.getCurrentPersistenceContext();
		if (uow != null && uow.hasChanges()) {
			getLiveObjectCache().clearInferredObjects();
		}
	}

	/**
	 * Connect the session to the ontology. This method actually starts up the
	 * connection pools and sets up ontology accessor.
	 */
	public void connect() {
		// TODO
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
		accessor.close();
	}

	public void releaseClientSession(ClientSession session) {
		// TODO
	}

	public void removeObjectFromCache(Object object) {
		if (object == null) {
			return;
		}
		getLiveObjectCache().removeObjectFromCache(object);
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
		T result = this.accessor.readEntity(cls, primaryKey);
		getLiveObjectCache().addObjectIntoCache(result);
		return result;
	}

	public Set<Class<?>> getManagedTypes() {
		return this.managedClasses;
	}

	@Override
	Metamodel getMetamodel() {
		return metamodel;
	}
}
