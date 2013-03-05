package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Vector;
import java.util.logging.Level;

import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.owlapi.EntityManagerImpl.State;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.exceptions.MetamodelNotSetException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class UnitOfWorkImpl extends AbstractSession implements UnitOfWork {

	private Map<Object, Object> cloneMapping;
	private Map<Object, Object> cloneToOriginals;
	private Map<Object, Object> deletedObjects;
	private Map<Object, Object> newObjectsCloneToOriginal;
	private Map<Object, Object> newObjectsOriginalToClone;
	private Map<Object, Object> newObjectsKeyToClone;
	// A set of primary keys (IRI or URI), which are already used and
	// no other object can use them
	private Set<Object> usedPrimaryKeys;

	private boolean hasChanges;
	private boolean hasNew;
	private boolean hasDeleted;
	private boolean shouldReleaseAfterCommit;
	private boolean shouldClearCacheAfterCommit;

	private boolean isActive;

	private UnitOfWorkChangeSet uowChangeSet;

	private AbstractSession parent;
	private EntityManager entityManager;
	private Connection storageConnection;

	private MergeManager mergeManager;
	private CloneBuilder cloneBuilder;
	private ChangeManager changeManager;
	/**
	 * This is a shortcut for the second level cache. Performance reasons (to
	 * prevent server session method call chain.
	 */
	protected final CacheManager cacheManager;

	public UnitOfWorkImpl(AbstractSession parent) {
		this.parent = parent;
		this.entityManager = null;
		this.isActive = true;
		this.cloneBuilder = new CloneBuilderImpl(this);
		this.cacheManager = parent.getLiveObjectCache();
		this.storageConnection = acquireConnection();
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

	/**
	 * {@inheritDoc}
	 */
	public <T> T readObject(Class<T> cls, Object primaryKey) {
		if (cls == null || primaryKey == null) {
			return null;
		}
		// First try to find the object among new uncommitted objects
		Object result = getNewObjectsKeyToClone().get(primaryKey);
		if (result != null) {
			// The result can be returned, since it is already registered in
			// this UOW
			return cls.cast(result);
		}
		// Search the cache
		result = getObjectFromCache(cls, primaryKey);
		if (result == null) {
			// The object is not in the session cache, so search the ontology
			result = storageFind(cls, primaryKey);
		}
		if (result == null) {
			return null;
		}
		Object clone = registerExistingObject(result);
		checkForCollections(clone);
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
				Object original = getCloneToOriginals().get(clone);
				if (original == null) {
					throw new OWLPersistenceException("Cannot find an original for clone!");
				}
				toDelete.put(clone, original);
			}
			changeSet.addDeletedObjects(toDelete);
		}
		if (hasChanges()) {
			try {
				for (Object clone : getCloneMapping().keySet()) {
					if (getDeletedObjects().containsKey(clone)) {
						// Make sure deleted objects are not persisted again
						continue;
					}
					Object original = getCloneToOriginals().get(clone);
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
			Object original = getNewObjectsCloneToOriginal().get(clone);
			if (original == null) {
				original = this.cloneBuilder.buildClone(clone);
			}
			if (original == null || clone == null) {
				throw new OWLPersistenceException(
						"Error while calculating changes for new objects. Original or clone not found.");
			}
			getNewObjectsCloneToOriginal().put(clone, original);
			getNewObjectsOriginalToClone().put(original, clone);
			ObjectChangeSet oChangeSet = new ObjectChangeSetImpl(original, clone, true, changeSet);
			changeSet.addNewObjectChangeSet(oChangeSet);
		}
	}

	public void clear() {
		this.cloneMapping = null;
		this.cloneToOriginals = null;
		this.deletedObjects = null;
		this.newObjectsCloneToOriginal = null;
		this.newObjectsOriginalToClone = null;
		this.newObjectsKeyToClone = null;
		this.usedPrimaryKeys = null;
		this.hasChanges = false;
		this.hasDeleted = false;
		this.hasNew = false;
	}

	public boolean contains(Object entity) {
		return (getCloneMapping().containsKey(entity) && !getDeletedObjects().containsKey(entity));
	}

	public void commit() {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("UnitOfWork commit started.");
		}
		if (!isActive()) {
			throw new OWLPersistenceException("Cannot commit inactive Unit of Work!");
		}
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
		for (Object clone : getCloneMapping().keySet()) {
			removeIndirectCollections(clone);
		}
		getNewObjectsCloneToOriginal().clear();
		getNewObjectsOriginalToClone().clear();
		getNewObjectsKeyToClone().clear();
		getDeletedObjects().clear();
		getCloneToOriginals().clear();
		getCloneMapping().clear();
		this.hasChanges = false;
		this.hasDeleted = false;
		this.hasNew = false;
		this.cloneBuilder.reset();
		this.uowChangeSet = null;
		if (shouldClearCacheAfterCommit) {
			cacheManager.acquireWriteLock();
			cacheManager.evictAll();
			cacheManager.releaseWriteLock();
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
			calculateChanges(this.uowChangeSet, getCloneMapping());
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
		if (entity == null) {
			return null;
		}
		if (getDeletedObjects().containsKey(entity)) {
			return State.REMOVED;
		} else if (getCloneToOriginals().containsKey(entity)) {
			return State.MANAGED;
		} else if (storageContains(getIdentifier(entity))) {
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
		Object original = getCloneToOriginals().get(clone);
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
		return getCloneToOriginals().containsValue(entity);
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
		Iterator<Entry<Object, Object>> it = getCloneToOriginals().entrySet().iterator();
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

	public Map<Object, Object> getCloneMapping() {
		if (this.cloneMapping == null) {
			this.cloneMapping = createMap();
		}
		return this.cloneMapping;
	}

	public Map<Object, Object> getCloneToOriginals() {
		if (this.cloneToOriginals == null) {
			this.cloneToOriginals = createMap();
		}
		return this.cloneToOriginals;
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

	protected Set<Object> getUsedPrimaryKeys() {
		if (this.usedPrimaryKeys == null) {
			this.usedPrimaryKeys = new HashSet<Object>();
		}
		return this.usedPrimaryKeys;
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
	 * Returns true if the given object is already managed. This means it is
	 * either in the shared session cache or in the cache of this UnitOfWork or
	 * it is a new object ready to be persisted on commit.
	 * 
	 * @param entity
	 *            Object
	 * @return boolean
	 */
	public boolean isObjectManaged(Object entity) {
		if (entity == null) {
			return false;
		}
		final IRI pk = getIdentifier(entity);
		if (pk == null) {
			throw new OWLPersistenceException("Unable to extract identified from entity " + entity);
		}
		return isObjectManaged(entity, pk);
	}

	private boolean isObjectManaged(Object entity, Object primaryKey) {
		Object original = getOriginal(entity);
		if (isInCache(entity.getClass(), primaryKey))
			return true;
		if (getCloneToOriginals().containsValue(original)) {
			return true;
		}
		if (getNewObjectsCloneToOriginal().containsKey(entity))
			return true;
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
			getMergeManager().mergeChangesFromChangeSet(getUowChangeSet());
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
			getCloneToOriginals().put(clone, original);
		}
		// Remove the clones and originals of the deleted objects from the
		// context
		Iterator<?> deletedIt = getUowChangeSet().getDeletedObjects().keySet().iterator();
		while (deletedIt.hasNext()) {
			ObjectChangeSet ochSet = (ObjectChangeSet) deletedIt.next();
			Object clone = ochSet.getCloneObject();
			getCloneMapping().remove(clone);
			getCloneToOriginals().remove(clone);
			Object orig = ochSet.getChangedObject();
			// TODO Removing can be done directly once change set contains
			// object's primary key
			removeObjectFromCache(orig);
		}
	}

	/**
	 * Merge the detached entity into the current persistence context. If the
	 * object is in the live object cache, the original is taken from there and
	 * the argument is considered a clone. If the object is not in the live
	 * object cache, the original is read from ontology.
	 */
	public void mergeDetached(Object entity) {
		if (entity == null) {
			throw new IllegalArgumentException("Null cannot be merged since it is not an entity.");
		}
		if (this.contains(entity)) {
			return;
		}
		final IRI iri = getIdentifier(entity);
		if (iri == null) {
			throw new OWLPersistenceException("The object is not an ontology entity.");
		}
		Object orig = null;
		final Class<?> cls = entity.getClass();
		orig = getObjectFromCache(cls, iri);
		if (orig == null) {
			orig = storageFind(cls, iri);
			if (orig == null) {
				throw new OWLPersistenceException(
						"The detached object is not in the ontology signature.");
			}
		}
		getUsedPrimaryKeys().add(iri);
		getCloneMapping().put(entity, entity);
		getCloneToOriginals().put(entity, orig);
		checkForCollections(entity);
		if (isInTransaction()) {
			persistChangeInTransaction(entity);
		}
		setHasChanges(true);
	}

	public Vector<Object> registerAllExistingObjects(Collection<Object> objects) {
		if (objects == null || objects.isEmpty()) {
			return new Vector<Object>();
		}
		Vector<Object> clones = new Vector<Object>();
		Iterator<?> it = objects.iterator();
		while (it.hasNext()) {
			Object original = it.next();
			Object clone = registerExistingObject(original);
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
	public Object registerExistingObject(Object object) {
		if (object == null) {
			return null;
		}
		if (getCloneToOriginals().containsValue(object)) {
			return getCloneForOriginal(object);
		}
		Object clone = this.cloneBuilder.buildClone(object);
		IRI iri = getIdentifier(clone);
		if (iri != null) {
			getUsedPrimaryKeys().add(iri);
		}
		getCloneMapping().put(clone, clone);
		getCloneToOriginals().put(clone, object);
		registerEntityWithContext(clone, this);
		storageRegisterCloneInConnection(object, clone);
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
		if (isInCache(object.getClass(), primaryKey)) {
			return registerExistingObject(object);
		}
		Object clone = readObject(object.getClass(), primaryKey);
		if (clone != null) {
			return clone;
		}
		return registerNewObject(primaryKey, object);
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

	/**
	 * Register a new object for persist.
	 * 
	 * @param id
	 *            IRI of the new entity
	 * @param entity
	 *            Object The entity to persist
	 */
	public Object registerNewObject(IRI id, Object entity) {
		if (entity == null) {
			throw new OWLPersistenceException("Cannot persist entity. IRI or entity is null!");
		}
		if (id == null) {
			// Check if the ID is generated
			final Class<?> cls = entity.getClass();
			final EntityType<?> eType = getMetamodel().entity(cls);
			if (!eType.getIdentifier().isGenerated()) {
				throw new OWLPersistenceException("The id for entity " + entity
						+ " is null and it is not specified as \'generated\' ");
			}
		}
		if ((storageContains(id) || isObjectManaged(entity, id) || primaryKeyAlreadyUsed(id))
				&& !entity.getClass().isEnum()) {
			throw new OWLPersistenceException("An entity with URI " + id
					+ " is already persisted within the context.");
		}
		storagePersist(id, entity);
		if (id == null) {
			// If the ID was null, extract it from the entity
			// It is present now
			id = getIdentifier(entity);
		}
		Object clone = entity;
		// Original is null until commit
		Object original = null;
		getCloneMapping().put(clone, clone);
		getNewObjectsCloneToOriginal().put(clone, original);
		registerEntityWithContext(clone, this);
		getUsedPrimaryKeys().add(id);
		getNewObjectsKeyToClone().put(id, clone);
		checkForCollections(clone);
		this.hasNew = true;
		return entity;
	}

	/**
	 * Check if the specified id has been already used. Returns true, if the id
	 * was used and hence it cannot be used anymore.
	 * 
	 * @param id
	 *            Usually URI or IRI
	 * @return boolean
	 */
	public boolean primaryKeyAlreadyUsed(Object id) {
		return getUsedPrimaryKeys().contains(id);
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
		Object registered = isRegistered(object);

		if (hasNew() && getNewObjectsCloneToOriginal().containsKey(registered)) {
			this.unregisterObject(registered);
			this.getNewObjectsKeyToClone().remove(primaryKey);
			this.getUsedPrimaryKeys().remove(primaryKey);
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
		getCloneMapping().remove(object);
		getCloneToOriginals().remove(object);

		getDeletedObjects().remove(object);
		if (hasNew()) {
			Object newOriginal = getNewObjectsCloneToOriginal().remove(object);
			if (newOriginal != null) {
				getNewObjectsOriginalToClone().remove(newOriginal);
			}
		}
		removeIndirectCollections(object);
	}

	/**
	 * This method checks if the given object is registered in this Unit of
	 * Work. The specified object may either be a clone or it might be a new
	 * object (then it is an original). In either case this method returns the
	 * clone of the registered object or null.
	 * 
	 * @param object
	 *            Object
	 * @return Clone of the registered object or null, if the object is not
	 *         registered.
	 */
	private Object isRegistered(Object object) {
		if (object == null) {
			return null;
		}
		// If we have a clone
		Object registered = getCloneMapping().get(object);
		if (registered != null) {
			return registered;
		}
		if (hasNew()) {
			registered = getNewObjectsOriginalToClone().get(object);
		}

		return registered;
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
	 * Loads lazily loaded field on the specified entity. </p>
	 * 
	 * @param entity
	 *            Entity
	 * @param fieldName
	 *            Name of the field to load
	 * @throws NullPointerException
	 *             If {@code entity} or {@code fieldName} is {@code null}
	 * @throws OWLPersistenceException
	 *             If an error during loading occurs
	 */
	public <T> void loadEntityField(T entity, String fieldName) {
		if (entity == null || fieldName == null) {
			throw new NullPointerException();
		}
		try {
			storageConnection.loadFieldValue(entity, fieldName);
			checkForCollections(entity);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
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
	 * This is just a delegate for the cache. It handles read lock acquiring and
	 * releasing.
	 * 
	 * @param cls
	 * @param primaryKey
	 * @return True if the cache contains an object with the specified
	 *         primaryKey
	 */
	private boolean isInCache(Class<?> cls, Object primaryKey) {
		assert cls != null;
		assert primaryKey != null;
		cacheManager.acquireReadLock();
		final boolean res = cacheManager.contains(cls, primaryKey);
		cacheManager.releaseReadLock();
		return res;
	}

	/**
	 * Get entity with the specified primary key from the cache. </p>
	 * 
	 * If the cache does not contain any object with the specified primary key
	 * and class, null is returned. This method is just a delegate for the cache
	 * methods, it handles locks.
	 * 
	 * @param cls
	 * @param primaryKey
	 * @return Cached object or null
	 */
	private Object getObjectFromCache(Class<?> cls, Object primaryKey) {
		assert cls != null;
		assert primaryKey != null;
		cacheManager.acquireReadLock();
		final Object entity = cacheManager.get(cls, primaryKey);
		cacheManager.releaseReadLock();
		return entity;
	}

	private IRI getIdentifier(Object entity) {
		assert entity != null;
		return EntityPropertiesUtils.getPrimaryKey(entity, getMetamodel());
	}

	private boolean storageContains(Object primaryKey) {
		assert primaryKey != null;
		try {
			return storageConnection.contains(primaryKey);
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private <T> T storageFind(Class<T> cls, Object primaryKey) {
		assert cls != null;
		assert primaryKey != null;
		try {
			return storageConnection.find(cls, primaryKey);
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

	private <T> void storagePersist(Object primaryKey, T entity) {
		assert entity != null;
		try {
			storageConnection.persist(primaryKey, entity);
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

	private void storageCommit() {
		try {
			storageConnection.commit();
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}

	private <T> void storageRegisterCloneInConnection(Object original, Object clone) {
		try {
			final Context ctx = storageConnection.getSaveContextFor(original);
			storageConnection.registerWithContext(clone, ctx.getUri());
		} catch (OntoDriverException e) {
			throw new OWLPersistenceException(e);
		}
	}
}
