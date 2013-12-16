package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Level;

import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.owlapi.EntityManagerImpl.State;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.exceptions.MetamodelNotSetException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class UnitOfWorkImpl extends AbstractSession implements UnitOfWork, QueryFactory {

	private final Map<Object, Object> cloneMapping;
	private final Map<Object, Object> cloneToOriginals;
	private Map<Object, Object> deletedObjects;
	private Map<Object, Object> newObjectsCloneToOriginal;
	private Map<Object, Object> newObjectsOriginalToClone;
	private Map<Object, Object> newObjectsKeyToClone;
	private final Map<URI, Set<Object>> contextToEntity;

	private boolean hasChanges;
	private boolean hasNew;
	private boolean hasDeleted;
	private boolean shouldReleaseAfterCommit;
	private boolean shouldClearCacheAfterCommit;
	private boolean useTransactionalOntology;

	private boolean isActive;
	private boolean inCommit;

	private UnitOfWorkChangeSet uowChangeSet;

	private AbstractSession parent;
	private EntityManager entityManager;
	private Connection storageConnection;

	private MergeManager mergeManager;
	private CloneBuilder cloneBuilder;
	private ChangeManager changeManager;
	private final QueryFactory queryFactory;
	/**
	 * This is a shortcut for the second level cache. Performance reasons (to
	 * prevent server session method call chain).
	 */
	protected final CacheManager cacheManager;

	public UnitOfWorkImpl(AbstractSession parent) {
		this.parent = parent;
		this.entityManager = null;
		this.isActive = true;
		this.cloneMapping = createMap();
		this.cloneToOriginals = createMap();
		this.contextToEntity = new HashMap<URI, Set<Object>>();
		this.cloneBuilder = new CloneBuilderImpl(this);
		this.cacheManager = parent.getLiveObjectCache();
		this.storageConnection = acquireConnection();
		this.queryFactory = new QueryFactoryImpl(this, storageConnection);
		this.inCommit = false;
		this.useTransactionalOntology = true;
	}

	/**
	 * This method returns null, since we don't support nested Units of Work
	 * yet.
	 */
	@Override
	public UnitOfWork acquireUnitOfWork() {
		return null;
	}

	@Override
	protected Connection acquireConnection() {
		return parent.acquireConnection();
	}

	@Override
	public <T> T readObject(Class<T> cls, Object primaryKey) {
		if (cls == null || primaryKey == null) {
			throw new NullPointerException("Null passed to readObject. cls = " + cls
					+ ", primaryKey = " + primaryKey);
		}
		return readObjectInternal(cls, primaryKey, null);
	}

	@Override
	public <T> T readObject(Class<T> cls, Object primaryKey, URI context) {
		if (cls == null || primaryKey == null || context == null) {
			throw new NullPointerException("Null passed to readObject. cls = " + cls
					+ ", primaryKey = " + primaryKey + ", context = " + context);
		}
		return readObjectInternal(cls, primaryKey, context);
	}

	private <T> T readObjectInternal(Class<T> cls, Object primaryKey, URI context) {
		assert cls != null;
		assert primaryKey != null;
		// First try to find the object among new uncommitted objects
		Object result = getNewObjectsKeyToClone().get(primaryKey);
		if (result != null && (context == null || isInContext(context, result))) {
			// The result can be returned, since it is already registered in
			// this UOW
			return cls.cast(result);
		}
		// Search the cache
		ContextToEntity<T> res = getObjectFromCache(context, cls, primaryKey);
		if (res.entity == null) {
			// The object is not in the session cache, so search the ontology
			res = storageFind(cls, primaryKey, context);
		}
		if (res.entity == null) {
			return null;
		}
		result = res.entity;
		context = res.contextUri;
		Object clone = registerExistingObject(result, context);
		checkForCollections(clone);
		registerWithContext(context, clone);
		return cls.cast(clone);
	}

	/**
	 * This method calculates the changes that were to the registered entities
	 * and adds these changes into the given change set for future commit to the
	 * ontology.
	 * 
	 * @param changeSet
	 * @param registeredObjects
	 */
	protected void calculateChanges(UnitOfWorkChangeSet changeSet,
			Map<Object, Object> registeredObjects) {
		if (hasNew()) {
			calculateNewObjects(changeSet);
		}
		if (hasDeleted()) {
			Map<Object, Object> toDelete = new HashMap<Object, Object>();
			for (Object clone : getDeletedObjects().keySet()) {
				Object original = cloneToOriginals.get(clone);
				if (original == null) {
					throw new OWLPersistenceException("Cannot find an original for clone!");
				}
				toDelete.put(clone, original);
			}
			changeSet.addDeletedObjects(toDelete);
		}
		if (hasChanges()) {
			try {
				for (Object clone : cloneMapping.keySet()) {
					if (getDeletedObjects().containsKey(clone)) {
						// Make sure deleted objects are not persisted again
						continue;
					}
					Object original = cloneToOriginals.get(clone);
					if (original == null && !getNewObjectsCloneToOriginal().containsKey(clone)) {
						throw new OWLPersistenceException("Cannot find an original for clone!");
					}
					if (original == null) {
						continue; // It was a new object
					}
					ObjectChangeSet chSet = cloneBuilder.createObjectChangeSet(original, clone,
							changeSet);
					chSet = getChangeManager().calculateChanges(chSet);
					if (chSet != null) {
						changeSet.addObjectChangeSet(chSet);
					}
				}
			} catch (IllegalAccessException e) {
				throw new OWLPersistenceException(e);
			} catch (IllegalArgumentException e) {
				throw new OWLPersistenceException(e);
			} catch (OWLInferredAttributeModifiedException e) {
				LOG.severe("Inferred attribute modified. This transaction won't be able to commit.");
				if (entityManager != null) {
					entityManager.getTransaction().setRollbackOnly();
				}
			}
		}
	}

	/**
	 * Create object change sets for the new objects and adds them into our
	 * UnitOfWorkChangeSet.
	 * 
	 * @param changeSet
	 *            UnitOfWorkChangeSet
	 */
	protected void calculateNewObjects(UnitOfWorkChangeSet changeSet) {
		if (changeSet == null) {
			return;
		}
		Iterator<?> it = getNewObjectsCloneToOriginal().keySet().iterator();
		while (it.hasNext()) {
			Object clone = it.next();
			final Context c = storageGetEntityContext(clone);
			Object original = getNewObjectsCloneToOriginal().get(clone);
			if (original == null) {
				original = this.cloneBuilder.buildClone(clone, c.getUri());
			}
			if (original == null || clone == null) {
				throw new OWLPersistenceException(
						"Error while calculating changes for new objects. Original or clone not found.");
			}
			getNewObjectsCloneToOriginal().put(clone, original);
			getNewObjectsOriginalToClone().put(original, clone);
			ObjectChangeSet oChangeSet = new ObjectChangeSetImpl(original, clone, true, changeSet,
					c.getUri());
			changeSet.addNewObjectChangeSet(oChangeSet);
		}
	}

	public void clear() {
		this.cloneMapping.clear();
		this.cloneToOriginals.clear();
		this.deletedObjects = null;
		this.newObjectsCloneToOriginal = null;
		this.newObjectsOriginalToClone = null;
		this.newObjectsKeyToClone = null;
		this.hasChanges = false;
		this.hasDeleted = false;
		this.hasNew = false;
	}

	public boolean contains(Object entity) {
		if (entity == null) {
			throw new NullPointerException();
		}
		return isObjectManaged(entity);
	}

	public void commit() {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("UnitOfWork commit started.");
		}
		if (!isActive()) {
			throw new OWLPersistenceException("Cannot commit inactive Unit of Work!");
		}
		this.inCommit = true;
		commitUnitOfWork();
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("UnitOfWork commit finished.");
		}
	}

	public void rollback() {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("UnitOfWork rollback started.");
		}
		if (!isActive()) {
			throw new OWLPersistenceException("Cannot rollback inactive Unit of Work!");
		}
		rollbackInternal();
		clear();
	}

	/**
	 * Commit this Unit of Work.
	 */
	protected void commitUnitOfWork() {
		commitToOntology();
		storageCommit();
		mergeChangesIntoParent();
		postCommit();
	}

	/**
	 * Clean up after the commit.
	 */
	private void postCommit() {
		// Remove indirect collections from clones
		for (Object clone : cloneMapping.keySet()) {
			removeIndirectCollections(clone);
		}
		getNewObjectsCloneToOriginal().clear();
		getNewObjectsOriginalToClone().clear();
		getNewObjectsKeyToClone().clear();
		getDeletedObjects().clear();
		cloneToOriginals.clear();
		cloneMapping.clear();
		this.hasChanges = false;
		this.hasDeleted = false;
		this.hasNew = false;
		this.inCommit = false;
		this.cloneBuilder.reset();
		this.uowChangeSet = null;
		if (shouldClearCacheAfterCommit) {
			cacheManager.acquireWriteLock();
			try {
				cacheManager.evictAll();
			} finally {
				cacheManager.releaseWriteLock();
			}
			this.shouldReleaseAfterCommit = true;
		}
	}

	/**
	 * If there are any changes, commit them to the ontology.
	 */
	protected void commitToOntology() {
		boolean hasChanges = this.hasNew || this.hasChanges || this.hasDeleted;
		if (hasChanges) {
			if (this.uowChangeSet == null) {
				this.uowChangeSet = new UnitOfWorkChangeSetImpl(this);
			}
			calculateChanges(this.uowChangeSet, cloneMapping);
		}
	}

	private void rollbackInternal() {
		try {
			storageConnection.rollback();
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	/**
	 * Creates and returns a new IdentityHashMap. This is a factory method.
	 * 
	 * @return Map<Object, Object>
	 */
	private Map<Object, Object> createMap() {
		return new IdentityHashMap<Object, Object>();
	}

	public AbstractSession getParent() {
		return this.parent;
	}

	/**
	 * Checks the state of the specified entity. Note that the entity is
	 * actually a clone.
	 * 
	 * @param entity
	 *            Object
	 * @return The state of the specified entity
	 */
	public State getState(Object entity) {
		return getState(entity, null);
	}

	/**
	 * Checks the state of the specified entity with regards to the specified
	 * context.
	 * 
	 * @param entity
	 *            Object
	 * @param contextUri
	 *            URI of context
	 * @return The state of the specified entity
	 */
	public State getState(Object entity, URI contextUri) {
		if (entity == null) {
			throw new NullPointerException();
		}
		if (getDeletedObjects().containsKey(entity)) {
			return State.REMOVED;
		} else if (getNewObjectsCloneToOriginal().containsKey(entity)) {
			return State.NEW;
		} else if (cloneMapping.containsKey(entity)
				&& (contextUri == null || isInContext(contextUri, entity))) {
			return State.MANAGED;
		} else if (storageContains(getIdentifier(entity), contextUri)) {
			return State.DETACHED;
		} else {
			return State.NEW;
		}
	}

	/**
	 * Tries to find the original object for the given clone. It searches the
	 * existing objects, new objects and deleted objects.
	 * 
	 * @param clone
	 *            Object
	 * @return The original object for the given clone
	 */
	public Object getOriginal(Object clone) {
		if (clone == null) {
			return null;
		}
		Object original = cloneToOriginals.get(clone);
		if (original == null) {
			original = getNewObjectsCloneToOriginal().get(clone);
		}
		return original;
	}

	/**
	 * Check if this UnitOfWork contains this original entity. This method is
	 * used by the CloneBuilder so it does not have to clone already managed
	 * referenced objects.
	 * 
	 * @param entity
	 *            The original entity.
	 * @return True if the original is managed in this UnitOfWork.
	 */
	boolean containsOriginal(Object entity) {
		if (entity == null) {
			return false;
		}
		return cloneToOriginals.containsValue(entity);
	}

	/**
	 * Finds clone for the specified original. This method assumes that the
	 * original is managed in this persistence context (UnitOfWork). However, if
	 * not, this method just goes through all the managed objects and if it does
	 * not find match, returns null.
	 * 
	 * @param original
	 *            The original object whose clone we are looking for.
	 * @return The clone or null, if there is none.
	 */
	Object getCloneForOriginal(Object original) {
		Iterator<Entry<Object, Object>> it = cloneToOriginals.entrySet().iterator();
		while (it.hasNext()) {
			Entry<Object, Object> entry = it.next();
			// We use IdentityMap, so we can use ==
			if (entry.getValue() == original) {
				return entry.getKey();
			}
		}
		return null;
	}

	public boolean hasDeleted() {
		return this.hasDeleted;
	}

	public boolean hasChanges() {
		return this.hasChanges || this.hasDeleted || this.hasNew;
	}

	public boolean hasNew() {
		return this.hasNew;
	}

	public void setHasChanges(boolean hasChanges) {
		this.hasChanges = hasChanges;
	}

	public Map<Object, Object> getDeletedObjects() {
		if (this.deletedObjects == null) {
			this.deletedObjects = createMap();
		}
		return this.deletedObjects;
	}

	public Map<Object, Object> getNewObjectsCloneToOriginal() {
		if (this.newObjectsCloneToOriginal == null) {
			this.newObjectsCloneToOriginal = createMap();
		}
		return this.newObjectsCloneToOriginal;
	}

	public Map<Object, Object> getNewObjectsOriginalToClone() {
		if (this.newObjectsOriginalToClone == null) {
			this.newObjectsOriginalToClone = createMap();
		}
		return this.newObjectsOriginalToClone;
	}

	public Map<Object, Object> getNewObjectsKeyToClone() {
		if (this.newObjectsKeyToClone == null) {
			// Cannot use identity map, since it compares the key references
			// which may not be the same
			this.newObjectsKeyToClone = new HashMap<Object, Object>();
		}
		return newObjectsKeyToClone;
	}

	@Override
	public CacheManager getLiveObjectCache() {
		return parent.getLiveObjectCache();
	}

	/**
	 * Get a set of classes managed in this persistence unit.
	 * 
	 * @return Set of managed classes.
	 */
	public Set<Class<?>> getManagedTypes() {
		if (this.parent == null) {
			return Collections.emptySet();
		}
		return this.parent.getManagedTypes();
	}

	public ChangeManager getChangeManager() {
		if (this.changeManager == null) {
			this.changeManager = new ChangeManagerImpl();
		}
		return this.changeManager;
	}

	public MergeManager getMergeManager() {
		if (this.mergeManager == null) {
			this.mergeManager = new MergeManagerImpl(this);
		}
		return this.mergeManager;
	}

	public UnitOfWorkChangeSet getUowChangeSet() {
		if (this.uowChangeSet == null) {
			this.uowChangeSet = new UnitOfWorkChangeSetImpl(this);
		}
		return uowChangeSet;
	}

	public boolean isActive() {
		return this.isActive;
	}

	/**
	 * Returns true if the given clone represents a newly created object.
	 * Otherwise returns false.
	 * 
	 * @param clone
	 *            Object
	 * @return boolean
	 */
	public boolean isObjectNew(Object clone) {
		if (clone == null)
			return false;
		return getNewObjectsCloneToOriginal().containsKey(clone);
	}

	/**
	 * Returns true if the given object is already managed.
	 * 
	 * @param entity
	 *            Object
	 * @return boolean
	 */
	public boolean isObjectManaged(Object entity) {
		if (entity == null) {
			throw new NullPointerException();
		}
		return (cloneMapping.containsKey(entity) && !getDeletedObjects().containsKey(entity));
	}

	@Override
	public boolean isContextConsistent(URI contextUri) {
		if (contextUri == null) {
			throw new NullPointerException();
		}
		try {
			return storageConnection.isConsistent(contextUri);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private boolean doesEntityExist(Object entity, Object primaryKey, URI context) {
		assert entity != null;
		assert context != null;
		if (cloneMapping.containsKey(entity) && !getDeletedObjects().containsKey(entity)
				&& isInContext(context, entity)) {
			return true;
		}
		if (primaryKey != null) {
			if (context == null) {
				context = getContexts().get(0).getUri();
			}
			getLiveObjectCache().acquireReadLock();
			try {
				return getLiveObjectCache().contains(context, entity.getClass(), primaryKey);
			} finally {
				getLiveObjectCache().releaseReadLock();
			}
		}
		return false;
	}

	/**
	 * Persist changes made to the specified entity to the underlying
	 * transactional ontology.
	 * 
	 * @param entity
	 *            The entity with changes
	 * @throws IllegalStateException
	 *             If this {@code UnitOfWork} is not in transaction
	 */
	public void persistChangeInTransaction(Object entity) {
		if (!isInTransaction()) {
			throw new IllegalStateException("This unit of work is not in a transaction.");
		}
		storageMerge(getIdentifier(entity), entity);
		setHasChanges(true);
		// Let's see how this works
		checkForCollections(entity);
	}

	/**
	 * Merge the changes from this Unit of Work's change set into the parent
	 * session and to the original objects. Also mark new objects as existing,
	 * since they are already persisted.
	 */
	public void mergeChangesIntoParent() {
		if (hasChanges() && getUowChangeSet() != null) {
			getLiveObjectCache().acquireWriteLock();
			try {
				getMergeManager().mergeChangesFromChangeSet(getUowChangeSet());
			} finally {
				getLiveObjectCache().releaseWriteLock();
			}
		}
		// Mark new persistent object as existing and managed
		Iterator<?> it = getNewObjectsCloneToOriginal().keySet().iterator();
		while (it.hasNext()) {
			Object clone = it.next();
			Object original = getNewObjectsCloneToOriginal().get(clone);
			if (original != null) {
				getNewObjectsOriginalToClone().remove(original);
			}
			it.remove();
			// getNewObjectsCloneToOriginal().remove(clone);
			// Clones are already in cloneMapping, so just put them here
			cloneToOriginals.put(clone, original);
		}
		// Remove the clones and originals of the deleted objects from the
		// context
		Iterator<?> deletedIt = getUowChangeSet().getDeletedObjects().keySet().iterator();
		while (deletedIt.hasNext()) {
			ObjectChangeSet ochSet = (ObjectChangeSet) deletedIt.next();
			Object clone = ochSet.getCloneObject();
			cloneMapping.remove(clone);
			cloneToOriginals.remove(clone);
		}
	}

	@Override
	public <T> T mergeDetached(T entity) {
		if (entity == null) {
			throw new NullPointerException("Null cannot be merged since it is not an entity.");
		}
		return mergeDetachedInternal(entity, null);
	}

	@Override
	public <T> T mergeDetached(T entity, URI contextUri) {
		if (entity == null || contextUri == null) {
			throw new NullPointerException("Null passed to mergeDetached: entity = " + entity
					+ ", contextUri = " + contextUri);
		}
		return mergeDetachedInternal(entity, contextUri);
	}

	private <T> T mergeDetachedInternal(T entity, URI contextUri) {
		assert entity != null;
		final IRI iri = getIdentifier(entity);
		if (!storageContains(iri, contextUri)) {
			throw new OWLPersistenceException("Entity " + entity + " not found in context "
					+ contextUri);
		}
		if (contextUri == null) {
			final ContextToEntity<?> c = storageFind(entity.getClass(), iri, null);
			contextUri = c.contextUri;
		}
		// This cast is OK, we just clone the entity instance
		final T clone = (T) registerExistingObject(entity, contextUri);

		if (isInTransaction()) {
			persistChangeInTransaction(clone);
		}
		cacheManager.acquireReadLock();
		try {
			if (cacheManager.contains(contextUri, clone.getClass(), iri)) {
				cacheManager.releaseReadLock();
				cacheManager.acquireWriteLock();
				try {
					cacheManager.evict(contextUri, entity.getClass(), iri);
					cacheManager.acquireReadLock();
				} finally {
					cacheManager.releaseWriteLock();
				}
			}
		} finally {
			cacheManager.releaseReadLock();
		}
		checkForCollections(clone);
		setHasChanges(true);
		return clone;
	}

	public Vector<Object> registerAllExistingObjects(Collection<Object> objects) {
		if (objects == null || objects.isEmpty()) {
			return new Vector<Object>();
		}
		Vector<Object> clones = new Vector<Object>();
		Iterator<?> it = objects.iterator();
		while (it.hasNext()) {
			Object original = it.next();
			URI context;
			try {
				context = storageConnection.getSaveContextFor(original).getUri();
			} catch (OntoDriverException e) {
				throw new OWLPersistenceException(e);
			}
			Object clone = registerExistingObject(original, context);
			clones.add(clone);
		}
		return clones;
	}

	public Vector<Object> registerAllObjects(Collection<Object> objects) {
		if (objects == null || objects.isEmpty()) {
			return new Vector<Object>();
		}
		Vector<Object> clones = new Vector<Object>();
		Iterator<?> it = objects.iterator();
		while (it.hasNext()) {
			Object original = it.next();
			Object clone = registerObject(original);
			clones.add(clone);
		}
		return clones;
	}

	/**
	 * {@inheritDoc}
	 */
	void registerEntityWithContext(Object entity, UnitOfWorkImpl uow) {
		parent.registerEntityWithContext(entity, uow);
	}

	/**
	 * {@inheritDoc}
	 */
	public Object registerExistingObject(Object object, URI contextUri) {
		if (object == null) {
			return null;
		}
		if (cloneToOriginals.containsValue(object)) {
			return getCloneForOriginal(object);
		}
		Object clone = this.cloneBuilder.buildClone(object, contextUri);
		cloneMapping.put(clone, clone);
		cloneToOriginals.put(clone, object);
		registerEntityWithContext(clone, this);
		storageRegisterCloneInConnection(clone, contextUri);
		return clone;
	}

	/**
	 * This method is not supposed to be used to register objects in this UoW,
	 * since it has to do a lot of checking and loading to register the object.
	 * {@link #registerExistingObject(Object)} or
	 * {@link #registerNewObject(IRI, Object)} should be used instead.
	 */
	public Object registerObject(Object object) {
		IRI primaryKey = getIdentifier(object);
		if (primaryKey == null) {
			throw new OWLPersistenceException("The specified object is not a valid entity.");
		}
		Object clone = readObject(object.getClass(), primaryKey);
		if (clone != null) {
			return clone;
		}
		registerNewObject(object);
		return null;
	}

	/**
	 * Release this Unit of Work. Releasing an active Unit of Work with
	 * uncommited changes causes the Unit of Work to try to write the changes
	 * into the ontology and then releasing.
	 */
	public void release() {
		writeUncommittedChanges();
		clear();
		if (storageConnection != null) {
			try {
				storageConnection.close();
			} catch (OntoDriverException e) {
				LOG.log(Level.SEVERE, "Exception caugth when closing connection.", e);
			}
		}
		this.isActive = false;
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("UnitOfWork released.");
		}
	}

	public Object revertObject(Object object) {
		ObjectChangeSet chSet = new ObjectChangeSetImpl(object, getOriginal(object), false, null);
		try {
			getChangeManager().calculateChanges(chSet);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (OWLInferredAttributeModifiedException e) {
			throw new OWLPersistenceException(e);
		}
		object = getMergeManager().mergeChangesOnObject(object, chSet);
		return object;
	}

	public void removeObjectFromCache(Object object) {
		if (object == null) {
			return;
		}
		final IRI primaryKey = getIdentifier(object);
		if (primaryKey == null) {
			return;
		}
		cacheManager.acquireWriteLock();
		cacheManager.evict(object.getClass(), primaryKey);
		cacheManager.releaseWriteLock();
	}

	@Override
	public void registerNewObject(Object entity) {
		if (entity == null) {
			throw new NullPointerException("Null passed to registerNewObject: entity " + entity);
		}
		registerNewObjectInternal(entity, null);
	}

	@Override
	public void registerNewObject(Object entity, URI context) {
		if (entity == null || context == null) {
			throw new NullPointerException("Null passed to registerNewObject: entity " + entity
					+ ", context = " + context);
		}
		registerNewObjectInternal(entity, context);
	}

	/**
	 * Registers the specified entity for persist in this Unit of Work.
	 * 
	 * @param entity
	 *            The entity to register
	 * @param context
	 *            URI of context. Optional
	 */
	private void registerNewObjectInternal(Object entity, URI context) {
		assert entity != null;
		IRI id = getIdentifier(entity);
		if (id == null) {
			// Check if the ID is generated
			final Class<?> cls = entity.getClass();
			final EntityType<?> eType = getMetamodel().entity(cls);
			if (!eType.getIdentifier().isGenerated()) {
				throw new OWLPersistenceException("The id for entity " + entity
						+ " is null and it is not specified as \'generated\' ");
			}
		}
		if (context == null) {
			context = storageGetEntityContext(entity).getUri();
		}
		if ((doesEntityExist(entity, id, context) || storageContains(id, context))
				&& !entity.getClass().isEnum()) {
			throw new OWLEntityExistsException("An entity with URI " + id
					+ " is already persisted in context " + context);
		}
		storagePersist(id, entity, context);
		if (id == null) {
			// If the ID was null, extract it from the entity
			// It is present now
			id = getIdentifier(entity);
		}
		Object clone = entity;
		// Original is null until commit
		Object original = null;
		cloneMapping.put(clone, clone);
		getNewObjectsCloneToOriginal().put(clone, original);
		registerEntityWithContext(clone, this);
		getNewObjectsKeyToClone().put(id, clone);
		checkForCollections(clone);
		this.hasNew = true;
	}

	/**
	 * Remove the specified entity from the ontology.
	 * 
	 * @param object
	 *            Clone of the object to delete
	 */
	public void removeObject(Object object) {
		if (object == null) {
			return;
		}
		if (!isObjectManaged(object)) {
			throw new OWLPersistenceException("Cannot remove object that is not managed!");
		}
		if (getDeletedObjects().containsKey(object)) {
			return;
		}
		final Object primaryKey = getIdentifier(object);

		if (hasNew() && getNewObjectsCloneToOriginal().containsKey(object)) {
			this.unregisterObject(object);
			this.getNewObjectsKeyToClone().remove(primaryKey);
		} else {
			this.getDeletedObjects().put(object, object);
			this.hasDeleted = true;
		}
		storageRemove(primaryKey, object);
	}

	/**
	 * Remove the registered object from this Unit of Work.
	 * 
	 * @param object
	 *            Clone of the original object
	 */
	public void unregisterObject(Object object) {
		if (object == null) {
			return;
		}
		cloneMapping.remove(object);
		cloneToOriginals.remove(object);

		getDeletedObjects().remove(object);
		if (hasNew()) {
			Object newOriginal = getNewObjectsCloneToOriginal().remove(object);
			if (newOriginal != null) {
				getNewObjectsOriginalToClone().remove(newOriginal);
			}
		}
		removeIndirectCollections(object);
	}

	public boolean shouldReleaseAfterCommit() {
		return shouldReleaseAfterCommit;
	}

	public void setShouldClearAfterCommit(boolean shouldClearCache) {
		this.shouldClearCacheAfterCommit = shouldClearCache;
	}

	public void setEntityManager(EntityManager entityManager) {
		this.entityManager = entityManager;
	}

	public void writeUncommittedChanges() {
		if (!hasChanges()) {
			return;
		}
		commitUnitOfWork();
	}

	@Override
	Metamodel getMetamodel() {
		return parent.getMetamodel();
	}

	/**
	 * {@inheritDoc}
	 */
	public boolean isInTransaction() {
		if (entityManager == null) {
			return false;
		}
		return entityManager.getTransaction().isActive();
	}

	/**
	 * Returns {@code true} if this UoW is currently committing changes.
	 * 
	 * @return
	 */
	public boolean isInCommit() {
		return inCommit;
	}

	/**
	 * Checks whether the specified {@code cls} is an entity type.
	 * 
	 * @param cls
	 *            Class
	 * @return {@code true} if the {@code cls} is a managed type, {@code false}
	 *         otherwise
	 */
	public boolean isManagedType(Class<?> cls) {
		if (cls == null) {
			return false;
		}
		return getManagedTypes().contains(cls);
	}

	/**
	 * Loads lazily loaded field on the specified entity. </p>
	 * 
	 * @param entity
	 *            Entity
	 * @param field
	 *            The field to load
	 * @throws NullPointerException
	 *             If {@code entity} or {@code fieldName} is {@code null}
	 * @throws OWLPersistenceException
	 *             If an error during loading occurs
	 */
	public <T> void loadEntityField(T entity, Field field) {
		if (entity == null || field == null) {
			throw new NullPointerException();
		}
		try {
			storageConnection.loadFieldValue(entity, field);
			final URI context = storageConnection.getSaveContextFor(entity).getUri();
			final Class<?> cls = field.getType();
			if (isManagedType(cls)) {
				final Object orig = field.get(entity);
				final Object clone = registerExistingObject(orig, context);
				field.set(entity, clone);
			}
			checkForCollections(entity);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalArgumentException e) {
			throw new OWLPersistenceException(e);
		} catch (IllegalAccessException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public void setUseTransactionalOntologyForQueryProcessing() {
		this.useTransactionalOntology = true;
	}

	@Override
	public boolean useTransactionalOntologyForQueryProcessing() {
		return useTransactionalOntology;
	}

	@Override
	public void setUseBackupOntologyForQueryProcessing() {
		this.useTransactionalOntology = false;
	}

	@Override
	public boolean useBackupOntologyForQueryProcessing() {
		return !useTransactionalOntology;
	}

	@Override
	public Query<List<String>> createNativeQuery(String sparql, URI contextUri) {
		return queryFactory.createNativeQuery(sparql, contextUri);
	}

	@Override
	public <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass, URI contextUri) {
		return queryFactory.createNativeQuery(sparql, resultClass, contextUri);
	}

	@Override
	public Query createQuery(String query, URI contextUri) {
		return queryFactory.createQuery(query, contextUri);
	}

	@Override
	public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass, URI contextUri) {
		return queryFactory.createQuery(query, resultClass, contextUri);
	}

	/**
	 * Check if the specified entity contains a collection. If so, replace it
	 * with its indirect representation so that changes in that collection can
	 * be tracked.
	 * 
	 * @param entity
	 *            The entity to check
	 */
	private void checkForCollections(Object entity) {
		Field[] fields = entity.getClass().getDeclaredFields();
		for (Field f : fields) {
			setIndirectCollectionIfPresent(entity, f);
		}
	}

	/**
	 * Create and set indirect collection on the specified entity field.</p>
	 * 
	 * If the specified field is of Collection type and it is not already an
	 * indirect collection, create new one and set it as the value of the
	 * specified field on the specified entity.
	 * 
	 * @param entity
	 *            The entity collection will be set on
	 * @param field
	 *            The field to set
	 * @throws IllegalArgumentException
	 *             Reflection
	 * @throws IllegalAccessException
	 *             Reflection
	 */
	public void setIndirectCollectionIfPresent(Object entity, Field field) {
		if (entity == null || field == null) {
			throw new NullPointerException();
		}
		try {
			if (Collection.class.isAssignableFrom(field.getType())) {
				if (!field.isAccessible()) {
					field.setAccessible(true);
				}
				Collection<?> col = (Collection<?>) field.get(entity);
				if (col == null || col instanceof IndirectCollection) {
					return;
				}
				Object indirectCollection = ((CloneBuilderImpl) cloneBuilder)
						.createIndirectCollection(col, entity);
				field.set(entity, indirectCollection);
			}
		} catch (IllegalAccessException e) {
			LOG.severe("Unable to set indirect collection on entity " + entity);
			throw new OWLPersistenceException(e);
		}
	}

	/**
	 * Remove indirect collection implementations from the specified entity (if
	 * present).
	 * 
	 * @param entity
	 *            The entity to remove indirect collections from
	 */
	private void removeIndirectCollections(Object entity) {
		Field[] fields = entity.getClass().getDeclaredFields();
		try {
			for (Field f : fields) {
				if (Collection.class.isAssignableFrom(f.getType())) {
					if (!f.isAccessible()) {
						f.setAccessible(true);
					}
					Collection<?> col = (Collection<?>) f.get(entity);
					if (col == null || !(col instanceof IndirectCollection)) {
						continue;
					}
					IndirectCollection indCol = (IndirectCollection) col;
					f.set(entity, indCol.getReferencedCollection());
				}
			}
		} catch (IllegalAccessException e) {
			LOG.severe("Unable to set indirect collection on entity " + entity);
			throw new OWLPersistenceException(e);
		}
	}

	/**
	 * Get entity with the specified primary key from the cache. </p>
	 * 
	 * If the cache does not contain any object with the specified primary key
	 * and class, null is returned. This method is just a delegate for the cache
	 * methods, it handles locks.
	 * 
	 * @param contextUri
	 * @param cls
	 * @param primaryKey
	 * @return Cached object or null
	 */
	private <T> ContextToEntity<T> getObjectFromCache(URI contextUri, Class<T> cls,
			Object primaryKey) {
		assert cls != null;
		assert primaryKey != null;
		cacheManager.acquireReadLock();
		try {
			ContextToEntity<T> res = null;
			T entity = null;
			if (contextUri != null) {
				entity = cacheManager.get(contextUri, cls, primaryKey);
				res = new ContextToEntity<T>(contextUri, entity);
				return res;
			} else {
				for (Context ctx : getContexts()) {
					entity = cacheManager.get(ctx.getUri(), cls, primaryKey);
					if (entity != null) {
						res = new ContextToEntity<T>(ctx.getUri(), entity);
						return res;
					}
				}
			}
			res = new ContextToEntity<T>(null, null);
			return res;
		} finally {
			cacheManager.releaseReadLock();
		}
	}

	void putObjectIntoCache(Object primaryKey, Object entity, URI contextUri) {
		cacheManager.acquireWriteLock();
		try {
			cacheManager.add(contextUri, primaryKey, entity);
		} finally {
			cacheManager.releaseWriteLock();
		}
	}

	private IRI getIdentifier(Object entity) {
		assert entity != null;
		return EntityPropertiesUtils.getPrimaryKey(entity, getMetamodel());
	}

	private void registerWithContext(URI contextUri, Object entity) {
		assert contextUri != null;
		assert entity != null;
		Set<Object> set = contextToEntity.get(contextUri);
		if (set == null) {
			set = new HashSet<Object>();
			contextToEntity.put(contextUri, set);
		}
		set.add(entity);
	}

	private boolean isInContext(URI contextUri, Object entity) {
		assert contextUri != null;
		assert entity != null;
		if (!contextToEntity.containsKey(contextUri)) {
			return false;
		}
		final Set<Object> set = contextToEntity.get(contextUri);
		return set.contains(entity);
	}

	private boolean storageContains(Object primaryKey, URI context) {
		if (primaryKey == null) {
			return false;
		}
		try {
			if (context == null) {
				return storageConnection.contains(primaryKey);
			} else {
				return storageConnection.contains(primaryKey, context);
			}
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private <T> ContextToEntity<T> storageFind(Class<T> cls, Object primaryKey, URI context) {
		assert cls != null;
		assert primaryKey != null;
		try {
			ContextToEntity<T> res = null;
			T result = null;
			if (context == null) {
				result = storageConnection.find(cls, primaryKey);
				if (result != null) {
					context = storageConnection.getSaveContextFor(result).getUri();
				}
				res = new ContextToEntity<T>(context, result);
			} else {
				result = storageConnection.find(cls, primaryKey, context);
				res = new ContextToEntity<T>(context, result);
			}
			if (result != null) {
				putObjectIntoCache(primaryKey, result, context);
			}
			return res;
		} catch (MetamodelNotSetException e) {
			throw new OWLPersistenceException(e);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private <T> void storageMerge(Object primaryKey, T entity) {
		assert primaryKey != null;
		assert entity != null;
		try {
			storageConnection.merge(primaryKey, entity);
		} catch (MetamodelNotSetException e) {
			throw new OWLPersistenceException(e);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private <T> void storagePersist(Object primaryKey, T entity, URI context) {
		assert entity != null;
		try {
			if (context == null) {
				storageConnection.persist(primaryKey, entity);
			} else {
				storageConnection.persist(primaryKey, entity, context);
			}
		} catch (MetamodelNotSetException e) {
			throw new OWLPersistenceException(e);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private <T> void storageRemove(Object primaryKey, T entity) {
		assert primaryKey != null;
		assert entity != null;
		try {
			storageConnection.remove(primaryKey, entity);
		} catch (MetamodelNotSetException e) {
			throw new OWLPersistenceException(e);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private <T> Context storageGetEntityContext(T entity) {
		assert entity != null;
		try {
			return storageConnection.getSaveContextFor(entity);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private void storageCommit() {
		try {
			storageConnection.commit();
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private <T> void storageRegisterCloneInConnection(Object clone, URI contextUri) {
		assert clone != null;
		assert contextUri != null;
		try {
			storageConnection.registerWithContext(clone, contextUri);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	@Override
	public List<Context> getContexts() {
		try {
			return storageConnection.getContexts();
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private static final class ContextToEntity<T> {
		final URI contextUri;
		final T entity;

		ContextToEntity(URI contextUri, T entity) {
			this.contextUri = contextUri;
			this.entity = entity;
		}
	}
}
