package cz.cvut.kbss.owlpersistence.sessions;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.Set;
import java.util.Vector;

import org.semanticweb.owlapi.model.IRI;

import cz.cvut.kbss.owlpersistence.accessors.OntologyAccessor;
import cz.cvut.kbss.owlpersistence.model.EntityManager;
import cz.cvut.kbss.owlpersistence.model.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.owlpersistence.model.OWLPersistenceException;
import cz.cvut.kbss.owlpersistence.owlapi.EntityManagerImpl.State;

public class UnitOfWorkImpl extends AbstractSession implements UnitOfWork {

	protected Map<Object, Object> cloneMapping;
	protected Map<Object, Object> cloneToOriginals;
	protected Map<Object, Object> deletedObjects;
	protected Map<Object, Object> newObjectsCloneToOriginal;
	protected Map<Object, Object> newObjectsOriginalToClone;
	protected Map<Object, Object> newObjectsKeyToClone;
	// A set of primary keys (IRI or URI), which are already used and
	// no other object can use them
	protected Set<Object> usedPrimaryKeys;

	protected boolean hasChanges;
	protected boolean hasNew;
	protected boolean hasDeleted;
	protected boolean shouldReleaseAfterCommit;
	protected boolean shouldClearCacheAfterCommit;

	protected boolean isActive;

	protected UnitOfWorkChangeSet uowChangeSet;

	protected AbstractSession parent;
	protected EntityManager entityManager;

	protected MergeManager mergeManager;
	protected CommitManager commitManager;
	protected CloneBuilder cloneBuilder;
	protected ChangeManager changeManager;

	public UnitOfWorkImpl(AbstractSession parent) {
		this.parent = parent;
		this.entityManager = null;
		this.isActive = true;
		this.cloneBuilder = new CloneBuilderImpl(this);
	}

	/**
	 * This method returns null, since we don't support nested Units of Work
	 * yet.
	 */
	public UnitOfWork acquireUnitOfWork() {
		return null;
	}

	public Vector<?> readAllObjects(Class<?> domainClass) {
		// TODO Auto-generated method stub
		return this.parent.readAllObjects(domainClass);
	}

	public Object readObject(Class<?> domainClass) {
		// TODO
		return this.parent.readObject(domainClass);
	}

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
		result = getLiveObjectCache().getObjectByIRI(primaryKey);
		if (result == null) {
			// The object is not in the session cache, so search the ontology
			result = getOntologyAccessor().readEntity(cls, primaryKey);
		}
		if (result == null) {
			return null;
		}
		Object clone = registerExistingObject(result);
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
					throw new OWLPersistenceException(
							"Cannot find an original for clone!");
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
					if (original == null
							&& !getNewObjectsCloneToOriginal().containsKey(
									clone)) {
						throw new OWLPersistenceException(
								"Cannot find an original for clone!");
					}
					if (original == null) {
						continue; // It was a new object
					}
					ObjectChangeSet chSet = cloneBuilder.createObjectChangeSet(
							original, clone, changeSet);
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
				this.entityManager.getTransaction().setRollbackOnly();
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
			ObjectChangeSet oChangeSet = new ObjectChangeSetImpl(original,
					clone, true, changeSet);
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
		return (getCloneMapping().containsKey(entity) && !getDeletedObjects()
				.containsKey(entity));
	}

	public void commit() {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("UnitOfWork commit started.");
		}
		if (!isActive()) {
			throw new OWLPersistenceException(
					"Cannot commit inactive Unit of Work!");
		}
		commitUnitOfWork();
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("UnitOfWork commit finished.");
		}
	}

	/**
	 * Commit this Unit of Work.
	 */
	protected void commitUnitOfWork() {
		commitToOntology();
		mergeChangesIntoParent();
		postCommit();
	}

	/**
	 * Clean up after the commit.
	 */
	private void postCommit() {
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
			getLiveObjectCache().releaseCache();
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
			commitChangesToOntology(this.uowChangeSet);
		}
	}

	/**
	 * Commit any changes to the ontology.
	 * 
	 * @param changeSet
	 *            UnitOfWorkChangeSet
	 */
	protected void commitChangesToOntology(UnitOfWorkChangeSet changeSet) {
		if (!changeSet.hasChanges()) {
			this.hasChanges = false;
			return;
		}
		getCommitManager().commitChanges(changeSet);

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
		} else if (getOntologyAccessor().isInOntologySignature(
				getOntologyAccessor().getIdentifier(entity), true)) {
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
		Iterator<Entry<Object, Object>> it = getCloneToOriginals().entrySet()
				.iterator();
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

	public CommitManager getCommitManager() {
		if (this.commitManager == null) {
			this.commitManager = new CommitManagerImpl(getOntologyAccessor());
		}
		return this.commitManager;
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
		return this.parent.getLiveObjectCache();
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
		Object original = getOriginal(entity);
		if (getLiveObjectCache().containsObject(original))
			return true;
		if (getCloneToOriginals().containsValue(original)) {
			return true;
		}
		if (getNewObjectsCloneToOriginal().containsKey(entity))
			return true;
		return false;
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
		Iterator<?> deletedIt = getUowChangeSet().getDeletedObjects().keySet()
				.iterator();
		while (deletedIt.hasNext()) {
			ObjectChangeSet ochSet = (ObjectChangeSet) deletedIt.next();
			Object clone = ochSet.getCloneObject();
			getCloneMapping().remove(clone);
			getCloneToOriginals().remove(clone);
			Object orig = ochSet.getChangedObject();
			getLiveObjectCache().removeObjectFromCache(orig);
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
			return;
		}
		if (this.contains(entity)) {
			return;
		}
		final IRI iri = getOntologyAccessor().getIdentifier(entity);
		if (iri == null) {
			throw new OWLPersistenceException(
					"The object is not an ontology entity.");
		}
		Object orig = null;
		if (getLiveObjectCache().containsObjectByIRI(iri)) {
			orig = getLiveObjectCache().getObjectByIRI(iri);
		} else {
			orig = getOntologyAccessor().readEntity(entity.getClass(), iri);
			if (orig == null) {
				throw new OWLPersistenceException(
						"The detached object is not in the ontology signature.");
			}
		}
		getUsedPrimaryKeys().add(iri);
		getCloneMapping().put(entity, entity);
		getCloneToOriginals().put(entity, orig);
		setHasChanges(true);
	}

	public Vector<Object> registerAllObjects(Collection<Object> objects) {
		if (objects == null) {
			return null;
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

	public Object registerExistingObject(Object object) {
		if (object == null) {
			return null;
		}
		if (getCloneToOriginals().containsValue(object)) {
			return getCloneForOriginal(object);
		}
		Object clone = this.cloneBuilder.buildClone(object);
		IRI iri = getOntologyAccessor().getIdentifier(clone);
		if (iri != null) {
			getUsedPrimaryKeys().add(iri);
		}
		getCloneMapping().put(clone, clone);
		getCloneToOriginals().put(clone, object);
		setHasChanges(true); // TODO: Is this right?
		return clone;
	}

	/**
	 * This method is not supposed to be used to register objects in this UoW,
	 * since it has to do a lot of checking and loading to register the object.
	 * {@link #registerExistingObject(Object)} or
	 * {@link #registerNewObject(IRI, Object)} should be used instead.
	 */
	public Object registerObject(Object object) {
		if (getLiveObjectCache().containsObject(object)) {
			return registerExistingObject(object);
		}
		IRI primaryKey = getOntologyAccessor().getIdentifier(object);
		if (primaryKey == null) {
			throw new OWLPersistenceException(
					"The specified object is not a valid entity.");
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
		this.clear();
		this.isActive = false;
		if (LOG.isLoggable(Level.CONFIG)) {
			LOG.config("UnitOfWork released.");
		}
	}

	public Object revertObject(Object object) {
		ObjectChangeSet chSet = new ObjectChangeSetImpl(object,
				getOriginal(object), false, null);
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
		getLiveObjectCache().removeObjectFromCache(object);
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
		if (id == null || entity == null) {
			throw new OWLPersistenceException(
					"Cannot persist entity. IRI or entity is null!");
		}
		if ((getOntologyAccessor().isInOntologySignature(id, true)
				|| isObjectManaged(entity) || primaryKeyAlreadyUsed(id))
				&& !entity.getClass().isEnum()) {
			throw new OWLPersistenceException("An entity with URI " + id
					+ " is already persisted within the context.");
		}
		Object clone = entity;
		// Original is null until commit
		Object original = null;
		getCloneMapping().put(clone, clone);
		getNewObjectsCloneToOriginal().put(clone, original);
		getUsedPrimaryKeys().add(id);
		getNewObjectsKeyToClone().put(id, clone);
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
			throw new OWLPersistenceException(
					"Cannot remove object that is not managed!");
		}
		if (getDeletedObjects().containsKey(object)) {
			return;
		}
		Object registered = isRegistered(object);

		if (registered == null) {
			// If the object is not registered, we try to load it from the
			// ontology, register it and then delete.
			Object primaryKey = getOntologyAccessor().getIdentifier(object);
			// Try to get the primary key of the object
			if (primaryKey == null) {
				throw new OWLPersistenceException(
						"The specified object has not a valid IRI.");
			}
			Object clone = readObject(object.getClass(), primaryKey);
			if (clone == null) {
				throw new OWLPersistenceException(
						"The specified object is not an entity.");
			}
			getDeletedObjects().put(clone, clone);
			this.hasDeleted = true;
		} else {
			if (hasNew()
					&& getNewObjectsCloneToOriginal().containsKey(registered)) {
				this.unregisterObject(registered);
				Object id = getOntologyAccessor().getIdentifier(registered);
				this.getNewObjectsKeyToClone().remove(id);
				this.getUsedPrimaryKeys().remove(id);
			} else {
				this.getDeletedObjects().put(object, object);
				this.hasDeleted = true;
			}
		}
	}

	/**
	 * Remove the registered object from this Unit of Work and from the cache.
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
		return this.shouldReleaseAfterCommit;
	}

	public void setShouldClearAfterCommit(boolean shouldClearCache) {
		this.shouldClearCacheAfterCommit = shouldClearCache;
	}

	public void setEntityManager(EntityManager entityManager) {
		this.entityManager = entityManager;
	}

	@Override
	public OntologyAccessor getOntologyAccessor() {
		return this.parent.getOntologyAccessor();
	}

	public void writeUncommittedChanges() {
		if (!hasChanges()) {
			return;
		}
		commitToOntology();
		mergeChangesIntoParent();
		postCommit();
	}

}
