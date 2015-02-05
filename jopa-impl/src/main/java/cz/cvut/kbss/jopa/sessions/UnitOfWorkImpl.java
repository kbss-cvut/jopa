package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;
import cz.cvut.kbss.jopa.owlapi.AbstractEntityManager;
import cz.cvut.kbss.jopa.owlapi.EntityManagerImpl.State;
import cz.cvut.kbss.jopa.utils.CardinalityConstraintsValidation;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.ErrorUtils;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Level;

public class UnitOfWorkImpl extends AbstractSession implements UnitOfWork, QueryFactory {

    private final Map<Object, Object> cloneMapping;
    private final Map<Object, Object> cloneToOriginals;
    private Map<Object, Object> deletedObjects;
    private Map<Object, Object> newObjectsCloneToOriginal;
    private Map<Object, Object> newObjectsOriginalToClone;
    private Map<Object, Object> newObjectsKeyToClone;
    private RepositoryMap repoMap;

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
    private AbstractEntityManager entityManager;
    private final ConnectionWrapper storage;

    private final MergeManager mergeManager;
    private final CloneBuilder cloneBuilder;
    private final ChangeManager changeManager;
    private final QueryFactory queryFactory;
    /**
     * This is a shortcut for the second level cache.
     */
    private final CacheManager cacheManager;

    public UnitOfWorkImpl(AbstractSession parent) {
        this.parent = Objects.requireNonNull(parent, ErrorUtils.constructNPXMessage("parent"));
        this.cloneMapping = createMap();
        this.cloneToOriginals = createMap();
        this.repoMap = new RepositoryMap();
        repoMap.initDescriptors();
        this.cloneBuilder = new CloneBuilderImpl(this);
        this.cacheManager = parent.getLiveObjectCache();
        this.storage = acquireConnection();
        this.queryFactory = new QueryFactoryImpl(this, storage);
        this.mergeManager = new MergeManagerImpl(this);
        this.changeManager = new ChangeManagerImpl(this);
        this.inCommit = false;
        this.useTransactionalOntology = true;
        this.isActive = true;
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
    protected ConnectionWrapper acquireConnection() {
        final ConnectionWrapper conn = parent.acquireConnection();
        conn.setUnitOfWork(this);
        return conn;
    }

    @Override
    public <T> T readObject(Class<T> cls, Object primaryKey, Descriptor descriptor) {
        Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
        Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
        Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

        return readObjectInternal(cls, primaryKey, descriptor);
    }

    private <T> T readObjectInternal(Class<T> cls, Object primaryKey, Descriptor descriptor) {
        assert cls != null;
        assert primaryKey != null;
        assert descriptor != null;
        // First try to find the object among new uncommitted objects
        Object result = getNewObjectsKeyToClone().get(primaryKey);
        if (result != null && (isInRepository(descriptor, result))) {
            // The result can be returned, since it is already registered in
            // this UOW
            return cls.cast(result);
        }
        // Search the cache
        result = getObjectFromCache(cls, primaryKey, descriptor.getContext());
        if (result == null) {
            // The object is not in the session cache, so search the ontology
            result = storage.find(cls, primaryKey, descriptor, false);
        }
        if (result == null) {
            return null;
        }
        Object clone = registerExistingObject(result, descriptor);
        checkForCollections(clone);
        return cls.cast(clone);
    }

    /**
     * This method calculates the changes that were to the registered entities
     * and adds these changes into the given change set for future commit to the
     * ontology.
     */
    private void calculateChanges() {
        final UnitOfWorkChangeSet changeSet = getUowChangeSet();
        if (hasNew()) {
            calculateNewObjects(changeSet);
        }
        if (hasDeleted()) {
            calculateDeletedObjects(changeSet);
        }
//		if (hasChanges()) {
//			calculateModifiedObjects(changeSet);
//		}
    }

    /**
     * Create object change sets for the new objects and adds them into our
     * UnitOfWorkChangeSet.
     *
     * @param changeSet UnitOfWorkChangeSet
     */
    private void calculateNewObjects(UnitOfWorkChangeSet changeSet) {
        for (Object clone : getNewObjectsCloneToOriginal().keySet()) {
            final Descriptor c = getDescriptor(clone);
            Object original = getNewObjectsCloneToOriginal().get(clone);
            if (original == null) {
                original = this.cloneBuilder.buildClone(clone, c);
            }
            if (original == null) {
                throw new OWLPersistenceException(
                        "Error while calculating changes for new objects. Original not found.");
            }
            getNewObjectsCloneToOriginal().put(clone, original);
            getNewObjectsOriginalToClone().put(original, clone);
            changeSet.addNewObjectChangeSet(ChangeSetFactory.createObjectChangeSet(original, clone,
                    c));
        }
    }

    private void calculateDeletedObjects(final UnitOfWorkChangeSet changeSet) {
        for (Object clone : getDeletedObjects().keySet()) {
            Object original = cloneToOriginals.get(clone);
            if (original == null) {
                throw new OWLPersistenceException("Cannot find an original for clone!");
            }
            Descriptor descriptor = getDescriptor(clone);
            changeSet.addDeletedObjectChangeSet(ChangeSetFactory.createObjectChangeSet(original, clone,
                    descriptor));
        }
    }

    // TODO Remove this if all works fine with change recording during transaction
//	private void calculateModifiedObjects(final UnitOfWorkChangeSet changeSet) {
//		try {
//			for (Object clone : cloneMapping.keySet()) {
//				if (getDeletedObjects().containsKey(clone)) {
//					// Make sure deleted objects are not persisted again
//					continue;
//				}
//				Object original = cloneToOriginals.get(clone);
//				if (original == null && !getNewObjectsCloneToOriginal().containsKey(clone)) {
//					throw new OWLPersistenceException("Cannot find an original for clone!");
//				}
//				if (original == null) {
//					continue; // It was a new object
//				}
//				Descriptor descriptor = getDescriptor(clone);
//				ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(original, clone,
//						descriptor);
//				final boolean anyChanges = changeManager.calculateChanges(chSet);
//				if (anyChanges) {
//					changeSet.addObjectChangeSet(chSet);
//				}
//			}
//		} catch (IllegalAccessException | IllegalArgumentException e) {
//			throw new OWLPersistenceException(e);
//		}
//	}

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
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));

        return isObjectManaged(entity);
    }

    public void commit() {
        if (LOG.isLoggable(Level.FINE)) {
            LOG.fine("UnitOfWork commit started.");
        }
        if (!isActive()) {
            throw new IllegalStateException("Cannot commit inactive Unit of Work!");
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
            throw new IllegalStateException("Cannot rollback inactive Unit of Work!");
        }
        storage.rollback();
        clear();
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
        this.repoMap = new RepositoryMap();
        repoMap.initDescriptors();
        this.uowChangeSet = null;
        if (shouldClearCacheAfterCommit) {
            cacheManager.evictAll();
            this.shouldReleaseAfterCommit = true;
        }
    }

    /**
     * If there are any changes, commit them to the ontology.
     */
    protected void commitToOntology() {
        boolean hasChanges = this.hasNew || this.hasChanges || this.hasDeleted;
        if (hasChanges) {
            calculateChanges();
        }
        validateIntegrityConstraints();
        storageCommit();
    }

    private void validateIntegrityConstraints() {
        if (uowChangeSet == null) {
            return;
        }
        for (ObjectChangeSet changeSet : uowChangeSet.getNewObjects()) {
            CardinalityConstraintsValidation.validateCardinalityConstraints(changeSet.getCloneObject());
        }
        for (ObjectChangeSet changeSet : uowChangeSet.getExistingObjectsChanges()) {
            CardinalityConstraintsValidation.validateCardinalityConstraints(changeSet);
        }
    }

    private Map<Object, Object> createMap() {
        return new IdentityHashMap<>();
    }

    /**
     * Gets current state of the specified entity. </p>
     * <p/>
     * Note that since no repository is specified we can only determine if the
     * entity is managed or removed. Therefore if the case is different this
     * method returns State#NOT_MANAGED.
     *
     * @param entity The entity to check
     * @return State of the entity
     */
    public State getState(Object entity) {
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));

        if (getDeletedObjects().containsKey(entity)) {
            return State.REMOVED;
        } else if (getNewObjectsCloneToOriginal().containsKey(entity)) {
            return State.MANAGED_NEW;
        } else if (cloneMapping.containsKey(entity)) {
            return State.MANAGED;
        } else {
            return State.NOT_MANAGED;
        }
    }

    /**
     * Checks the state of the specified entity with regards to the specified
     * repository.
     *
     * @param entity     Object
     * @param descriptor Entity descriptor
     * @return The state of the specified entity
     */
    public State getState(Object entity, Descriptor descriptor) {
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
        Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

        if (getDeletedObjects().containsKey(entity)) {
            return State.REMOVED;
        } else if (cloneMapping.containsKey(entity) && isInRepository(descriptor, entity)) {
            if (getNewObjectsCloneToOriginal().containsKey(entity)) {
                return State.MANAGED_NEW;
            }
            return State.MANAGED;
        } else {
            return State.NOT_MANAGED;
        }
    }

    /**
     * Tries to find the original object for the given clone. It searches the
     * existing objects, new objects and deleted objects.
     *
     * @param clone Object
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
     * @param entity The original entity.
     * @return True if the original is managed in this UnitOfWork.
     */
    boolean containsOriginal(Object entity) {
        return entity != null && cloneToOriginals.containsValue(entity);
    }

    /**
     * Finds clone for the specified original. This method assumes that the
     * original is managed in this persistence context (UnitOfWork). However, if
     * not, this method just goes through all the managed objects and if it does
     * not find match, returns null.
     *
     * @param original The original object whose clone we are looking for.
     * @return The clone or null, if there is none.
     */
    Object getCloneForOriginal(Object original) {
        for (Entry<Object, Object> entry : cloneToOriginals.entrySet()) {
            // We use IdentityMap, so we can use ==
            if (entry.getValue() == original) {
                return entry.getKey();
            }
        }
        return null;
    }

    public boolean hasDeleted() {
        return hasDeleted;
    }

    public boolean hasChanges() {
        return hasChanges || hasDeleted || hasNew;
    }

    public boolean hasNew() {
        return hasNew;
    }

    public void setHasChanges() {
        this.hasChanges = true;
    }

    public Map<Object, Object> getDeletedObjects() {
        if (deletedObjects == null) {
            this.deletedObjects = createMap();
        }
        return deletedObjects;
    }

    public Map<Object, Object> getNewObjectsCloneToOriginal() {
        if (newObjectsCloneToOriginal == null) {
            this.newObjectsCloneToOriginal = createMap();
        }
        return newObjectsCloneToOriginal;
    }

    public Map<Object, Object> getNewObjectsOriginalToClone() {
        if (newObjectsOriginalToClone == null) {
            this.newObjectsOriginalToClone = createMap();
        }
        return newObjectsOriginalToClone;
    }

    public Map<Object, Object> getNewObjectsKeyToClone() {
        if (newObjectsKeyToClone == null) {
            // Cannot use identity map, since it compares the key references
            // which may not be the same
            this.newObjectsKeyToClone = new HashMap<>();
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
    @Override
    public Set<Class<?>> getManagedTypes() {
        return parent.getManagedTypes();
    }

    public UnitOfWorkChangeSet getUowChangeSet() {
        if (uowChangeSet == null) {
            this.uowChangeSet = ChangeSetFactory.createUoWChangeSet();
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
     * @param clone Object
     * @return boolean
     */
    public boolean isObjectNew(Object clone) {
        return clone != null && getNewObjectsCloneToOriginal().containsKey(clone);
    }

    /**
     * Returns true if the given object is already managed.
     *
     * @param entity Object
     * @return boolean
     */
    public boolean isObjectManaged(Object entity) {
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));

        return (cloneMapping.containsKey(entity) && !getDeletedObjects().containsKey(entity));
    }

    private boolean doesEntityExist(Object entity, Object primaryKey, Descriptor descriptor) {
        assert entity != null;
        assert descriptor != null;
        if (cloneMapping.containsKey(entity) && !getDeletedObjects().containsKey(entity)
                && isInRepository(descriptor, entity)) {
            return true;
        }
        return primaryKey != null
                && cacheManager.contains(entity.getClass(), primaryKey, descriptor.getContext());
    }

    /**
     * Persists changed value of the specified field.
     *
     * @param entity Entity with changes (the clone)
     * @param f      The field whose value has changed
     * @throws IllegalStateException If this UoW is not in transaction
     */
    public void attributeChanged(Object entity, Field f) {
        if (!isInTransaction()) {
            throw new IllegalStateException("This unit of work is not in a transaction.");
        }
        final Descriptor descriptor = getDescriptor(entity);
        if (descriptor == null) {
            throw new OWLPersistenceException("Unable to find repository for entity " + entity
                    + ". Is it registered in this UoW?");
        }
        storage.merge(entity, f, descriptor);
        createChangeRecord(entity, f, descriptor);
        setHasChanges();
        setIndirectCollectionIfPresent(entity, f);
    }

    private void createChangeRecord(Object clone, Field field, Descriptor descriptor) {
        final Object orig = getOriginal(clone);
        if (orig == null) {
            return;
        }
        ObjectChangeSet chSet = getUowChangeSet().getExistingObjectChanges(orig);
        if (chSet == null) {
            chSet = ChangeSetFactory.createObjectChangeSet(orig, clone, descriptor);
            getUowChangeSet().addObjectChangeSet(chSet);
        }
        if (!field.isAccessible()) {
            field.setAccessible(true);
        }
        try {
            chSet.addChangeRecord(new ChangeRecordImpl(field.getName(), field.get(clone)));
        } catch (IllegalAccessException e) {
            throw new OWLPersistenceException("Unable to read value of field " + field, e);
        }
    }

    /**
     * Merge the changes from this Unit of Work's change set into the server
     * session.
     */
    public void mergeChangesIntoParent() {
        if (hasChanges()) {
            mergeManager.mergeChangesFromChangeSet(getUowChangeSet());
        }
    }

    @Override
    public <T> T mergeDetached(T entity, Descriptor descriptor) {
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
        Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

        final Object pk = getIdentifier(entity);
        if (!storage.contains(pk, entity.getClass(), descriptor)) {
            registerNewObject(entity, descriptor);
            return entity;
        } else {
            return mergeDetachedInternal(entity, descriptor);
        }
    }

    private <T> T mergeDetachedInternal(T entity, Descriptor descriptor) {
        assert entity != null;
        final Object iri = getIdentifier(entity);
        final Class<T> entityCls = (Class<T>) entity.getClass();
        // Search the cache
        T original = getObjectFromCache(entityCls, iri, descriptor.getContext());
        if (original == null) {
            // The object is not in the session cache, so search the ontology
            original = storage.find(entityCls, iri, descriptor, true);
        }
        assert original != null;
        registerClone(entity, original, descriptor);
        try {
            // Merge only the changed attributes
            final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(original, entity, descriptor);
            changeManager.calculateChanges(chSet);
            final EntityType<?> et = getMetamodel().entity(entityCls);
            for (ChangeRecord record : chSet.getChanges().values()) {
                final Field field = et.getFieldSpecification(record.getAttributeName()).getJavaField();
                storage.merge(entity, field, descriptor);
            }
        } catch (OWLEntityExistsException e) {
            unregisterObject(entity);
            throw e;
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
        if (cacheManager.contains(entityCls, iri, descriptor.getContext())) {
            cacheManager.evict(entityCls, iri, descriptor.getContext());
        }
        setHasChanges();
        return entity;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void registerEntityWithPersistenceContext(Object entity, UnitOfWorkImpl uow) {
        parent.registerEntityWithPersistenceContext(entity, uow);
    }

    @Override
    void deregisterEntityFromPersistenceContext(Object entity, UnitOfWork uow) {
        parent.deregisterEntityFromPersistenceContext(entity, uow);
    }

    /**
     * {@inheritDoc}
     */
    public Object registerExistingObject(Object object, Descriptor descriptor) {
        if (object == null) {
            return null;
        }
        if (cloneToOriginals.containsValue(object)) {
            return getCloneForOriginal(object);
        }
        Object clone = this.cloneBuilder.buildClone(object, descriptor);
        assert clone != null;
        registerClone(clone, object, descriptor);
        return clone;
    }

    private void registerClone(Object clone, Object original, Descriptor descriptor) {
        cloneMapping.put(clone, clone);
        cloneToOriginals.put(clone, original);
        registerEntityWithPersistenceContext(clone, this);
        registerEntityWithOntologyContext(descriptor, clone);
    }

    /**
     * Release this Unit of Work. Releasing an active Unit of Work with
     * uncommitted changes causes all pending changes to be discarded.
     */
    public void release() {
        clear();
        storage.close();
        this.isActive = false;
        if (LOG.isLoggable(Level.CONFIG)) {
            LOG.config("UnitOfWork released.");
        }
    }

    @Override
    public <T> void revertObject(T object) {
        Objects.requireNonNull(object, ErrorUtils.constructNPXMessage("object"));

        if (!isObjectManaged(object) && !getDeletedObjects().containsKey(object)) {
            throw new IllegalArgumentException("The specified enity " + object
                    + " is not managed by this persistence context.");
        }
        final Descriptor descriptor = getDescriptor(object);
        if (descriptor == null) {
            throw new IllegalArgumentException("Unable to find entity " + object
                    + " in this persistence context.");
        }
        // To revert the object's state, just swap original and clone for change
        // calculation and merging so that the state of the original is merged
        // into the state of the clone
        final Object original = getOriginal(object);
        final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(object, original,
                descriptor);
        try {
            final boolean anyChanges = changeManager.calculateChanges(chSet);
            if (anyChanges) {
                mergeManager.mergeChangesOnObject(original, chSet);
            }
        } catch (IllegalAccessException | IllegalArgumentException e) {
            throw new OWLPersistenceException(e);
        }
    }

    @Override
    public void registerNewObject(Object entity, Descriptor descriptor) {
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
        Objects.requireNonNull(descriptor, ErrorUtils.constructNPXMessage("descriptor"));

        registerNewObjectInternal(entity, descriptor);
    }

    /**
     * Registers the specified entity for persist in this Unit of Work.
     *
     * @param entity     The entity to register
     * @param descriptor Entity descriptor, specifying optionally contexts into which the entity will be persisted
     */
    private void registerNewObjectInternal(Object entity, Descriptor descriptor) {
        assert entity != null;
        Object id = getIdentifier(entity);
        if (id == null) {
            final EntityType<?> eType = getMetamodel().entity(entity.getClass());
            EntityPropertiesUtils.verifyIdentifierIsGenerated(entity, eType);
        }
        if (doesEntityExist(entity, id, descriptor) && !entity.getClass().isEnum()) {
            throw new OWLEntityExistsException("An entity with URI " + id
                    + " is already persisted in repository " + descriptor);
        }
        storage.persist(id, entity, descriptor);
        if (id == null) {
            // If the ID was null, extract it from the entity
            // It is present now
            id = getIdentifier(entity);
        }
        // Original is null until commit
        cloneMapping.put(entity, entity);
        getNewObjectsCloneToOriginal().put(entity, null);
        registerEntityWithPersistenceContext(entity, this);
        registerEntityWithOntologyContext(descriptor, entity);
        getNewObjectsKeyToClone().put(id, entity);
        checkForCollections(entity);
        this.hasNew = true;
    }

    /**
     * Remove the specified entity from the ontology.
     *
     * @param entity Managed entity to delete
     */
    public void removeObject(Object entity) {
        if (entity == null) {
            return;
        }
        if (!isObjectManaged(entity)) {
            throw new IllegalArgumentException(
                    "Cannot remove entity which is not managed in the current persistence context.");
        }
        if (getDeletedObjects().containsKey(entity)) {
            return;
        }
        final Object primaryKey = getIdentifier(entity);
        final Descriptor descriptor = getDescriptor(entity);

        if (hasNew() && getNewObjectsCloneToOriginal().containsKey(entity)) {
            unregisterObject(entity);
            getNewObjectsKeyToClone().remove(primaryKey);
        } else {
            getDeletedObjects().put(entity, entity);
            this.hasDeleted = true;
        }
//		unregisterEntityFromOntologyContext(entity);
        storage.remove(primaryKey, entity.getClass(), descriptor);
    }

    /**
     * Remove the registered object from this Unit of Work.
     *
     * @param object Clone of the original object
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
        deregisterEntityFromPersistenceContext(object, this);
        unregisterEntityFromOntologyContext(object);
    }

    public boolean shouldReleaseAfterCommit() {
        return shouldReleaseAfterCommit;
    }

    public void setShouldClearAfterCommit(boolean shouldClearCache) {
        this.shouldClearCacheAfterCommit = shouldClearCache;
    }

    public void setEntityManager(AbstractEntityManager entityManager) {
        this.entityManager = entityManager;
    }

    public void writeUncommittedChanges() {
        if (!hasChanges()) {
            return;
        }
        commitUnitOfWork();
    }

    @Override
    public Metamodel getMetamodel() {
        return parent.getMetamodel();
    }

    @Override
    public boolean isTypeManaged(Class<?> cls) {
        return parent.isTypeManaged(cls);
    }

    @Override
    public boolean isInTransaction() {
        return entityManager != null && entityManager.getTransaction().isActive();
    }

    /**
     * Returns {@code true} if this UoW is currently committing changes.
     */
    public boolean isInCommit() {
        return inCommit;
    }

    @Override
    public <T> void loadEntityField(T entity, Field field) {
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
        Objects.requireNonNull(field, ErrorUtils.constructNPXMessage("field"));

        try {
            if (!field.isAccessible()) {
                field.setAccessible(true);
            }
            if (field.get(entity) != null) {
                return;
            }
            final Descriptor entityDescriptor = getDescriptor(entity);
            if (entityDescriptor == null) {
                throw new OWLPersistenceException(
                        "Unable to find repository identifier for entity " + entity
                                + ". Is it managed by this UoW?");
            }
            storage.loadFieldValue(entity, field, entityDescriptor);
            final Object orig = field.get(entity);
            final Object entityOriginal = getOriginal(entity);
            if (entityOriginal != null) {
                field.set(entityOriginal, orig);
            }
            final Descriptor fieldDescriptor = getFieldDescriptor(entity, field, entityDescriptor);
            final Object clone = cloneLoadedFieldValue(entity, field, fieldDescriptor, orig);
            field.set(entity, clone);
        } catch (IllegalArgumentException | IllegalAccessException e) {
            throw new OWLPersistenceException(e);
        }
    }

    private <T> Descriptor getFieldDescriptor(T entity, Field field, Descriptor entityDescriptor) {
        final EntityType<?> et = getMetamodel().entity(entity.getClass());
        final FieldSpecification<?, ?> fieldSpec = et
                .getFieldSpecification(field.getName());
        return entityDescriptor.getAttributeDescriptor(fieldSpec);
    }

    private <T> Object cloneLoadedFieldValue(T entity, Field field, final Descriptor fieldDescriptor,
                                             final Object fieldValueOrig) {
        Object clone;
        if (fieldValueOrig == null) {
            clone = null;
        } else {
            if (isTypeManaged(field.getType())) {
                clone = registerExistingObject(fieldValueOrig, fieldDescriptor);
                final URI fieldContext = fieldDescriptor.getContext();
                putObjectIntoCache(getIdentifier(clone), fieldValueOrig, fieldContext);
            } else {
                clone = cloneBuilder.buildClone(entity, field, fieldValueOrig, fieldDescriptor);
            }
        }
        return clone;
    }

    @Override
    public void removeObjectFromCache(Object toRemove, URI context) {
        Objects.requireNonNull(toRemove, ErrorUtils.constructNPXMessage("toRemove"));

        final Object primaryKey = getIdentifier(toRemove);
        cacheManager.evict(toRemove.getClass(), primaryKey, context);
    }

    @Override
    public boolean isConsistent(URI context) {
        return storage.isConsistent(context);
    }

    @Override
    public List<URI> getContexts() {
        return storage.getContexts();
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
    public Query<List<String>> createNativeQuery(String sparql) {
        return queryFactory.createNativeQuery(sparql);
    }

    @Override
    public <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass) {
        return queryFactory.createNativeQuery(sparql, resultClass);
    }

    @Override
    public Query createQuery(String query) {
        return queryFactory.createQuery(query);
    }

    @Override
    public <T> TypedQuery<T> createQuery(String query, Class<T> resultClass) {
        return queryFactory.createQuery(query, resultClass);
    }

    /**
     * Check if the specified entity contains a collection. If so, replace it
     * with its indirect representation so that changes in that collection can
     * be tracked.
     *
     * @param entity The entity to check
     */
    private void checkForCollections(Object entity) {
        Field[] fields = entity.getClass().getDeclaredFields();
        for (Field f : fields) {
            setIndirectCollectionIfPresent(entity, f);
        }
    }

    /**
     * Create and set indirect collection on the specified entity field.</p>
     * <p/>
     * If the specified field is of Collection type and it is not already an
     * indirect collection, create new one and set it as the value of the
     * specified field on the specified entity.
     *
     * @param entity The entity collection will be set on
     * @param field  The field to set
     * @throws IllegalArgumentException Reflection
     */
    public void setIndirectCollectionIfPresent(Object entity, Field field) {
        Objects.requireNonNull(entity, ErrorUtils.constructNPXMessage("entity"));
        Objects.requireNonNull(field, ErrorUtils.constructNPXMessage("field"));

        if (!field.isAccessible()) {
            field.setAccessible(true);
        }
        try {
            Object value = field.get(entity);
            if (value == null || value instanceof IndirectCollection) {
                return;
            }
            if (value instanceof Collection || value instanceof Map) {
                final Object indirectCollection = ((CloneBuilderImpl) cloneBuilder).createIndirectCollection(
                        value, entity, field);
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
     * @param entity The entity to remove indirect collections from
     */
    private void removeIndirectCollections(Object entity) {
        Field[] fields = entity.getClass().getDeclaredFields();
        try {
            for (Field f : fields) {
                if (!f.isAccessible()) {
                    f.setAccessible(true);
                }
                final Object ob = f.get(entity);
                if (ob == null) {
                    continue;
                }
                if (ob instanceof IndirectCollection) {
                    IndirectCollection<?> indCol = (IndirectCollection<?>) ob;
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
     * <p/>
     * If the cache does not contain any object with the specified primary key
     * and class, null is returned. This method is just a delegate for the cache
     * methods, it handles locks.
     *
     * @return Cached object or null
     */
    private <T> T getObjectFromCache(Class<T> cls, Object primaryKey, URI context) {
        assert cls != null;
        assert primaryKey != null;
        return cacheManager.get(cls, primaryKey, context);
    }

    public void putObjectIntoCache(Object primaryKey, Object entity, URI context) {
        cacheManager.add(primaryKey, entity, context);
    }

    private Object getIdentifier(Object entity) {
        assert entity != null;
        return EntityPropertiesUtils.getPrimaryKey(entity, getMetamodel());
    }

    private void unregisterEntityFromOntologyContext(Object entity) {
        assert entity != null;

        final Descriptor descriptor = repoMap.getEntityDescriptor(entity);
        if (descriptor == null) {
            throw new OWLPersistenceException("Fatal error, unable to find descriptor for entity " + entity);
        }

        repoMap.remove(descriptor, entity);
        repoMap.removeEntityToRepository(entity);
    }

    private void registerEntityWithOntologyContext(Descriptor repository, Object entity) {
        assert repository != null;
        assert entity != null;

        repoMap.add(repository, entity, null);
        repoMap.addEntityToRepository(entity, repository);
    }

    private boolean isInRepository(Descriptor descriptor, Object entity) {
        assert descriptor != null;
        assert entity != null;

        return repoMap.contains(descriptor, entity);
    }

    private Descriptor getDescriptor(Object entity) {
        assert entity != null;

        return repoMap.getEntityDescriptor(entity);
    }

    private void storageCommit() {
        try {
            storage.commit();
        } catch (OWLPersistenceException e) {
            entityManager.removeCurrentPersistenceContext();
            throw e;
        }
    }
}
