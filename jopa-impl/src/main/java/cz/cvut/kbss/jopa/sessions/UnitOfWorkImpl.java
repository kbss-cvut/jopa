/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectCollection;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.AbstractEntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerImpl.State;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.QueryImpl;
import cz.cvut.kbss.jopa.model.TypedQueryImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.EntityTypeImpl;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryFactory;
import cz.cvut.kbss.jopa.sessions.change.ChangeManagerImpl;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator;
import cz.cvut.kbss.jopa.utils.*;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;
import java.util.Map.Entry;

public class UnitOfWorkImpl extends AbstractSession implements UnitOfWork, QueryFactory, ConfigurationHolder, Wrapper {

    private final Map<Object, Object> cloneMapping;
    private final Map<Object, Object> cloneToOriginals;
    private final Map<Object, Object> keysToClones = new HashMap<>();
    private Map<Object, Object> deletedObjects;
    private Map<Object, Object> newObjectsCloneToOriginal;
    private Map<Object, Object> newObjectsOriginalToClone;
    private final Map<Object, Object> newObjectsKeyToClone = new HashMap<>();
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
    private final SparqlQueryFactory queryFactory;
    private final CollectionFactory collectionFactory;
    /**
     * This is a shortcut for the second level cache.
     */
    private final CacheManager cacheManager;

    public UnitOfWorkImpl(AbstractSession parent) {
        this.parent = Objects.requireNonNull(parent);
        this.cloneMapping = createMap();
        this.cloneToOriginals = createMap();
        this.repoMap = new RepositoryMap();
        repoMap.initDescriptors();
        this.cloneBuilder = new CloneBuilderImpl(this);
        this.collectionFactory = new CollectionFactory(this);
        this.cacheManager = parent.getLiveObjectCache();
        this.storage = acquireConnection();
        this.queryFactory = new SparqlQueryFactory(this, storage);
        this.mergeManager = new MergeManagerImpl(this);
        this.changeManager = new ChangeManagerImpl(this);
        this.inCommit = false;
        this.useTransactionalOntology = true;
        this.isActive = true;
    }

    CloneBuilder getCloneBuilder() {
        return cloneBuilder;
    }

    /**
     * This method returns null, since we don't support nested Units of Work yet.
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
        Objects.requireNonNull(cls, ErrorUtils.getNPXMessageSupplier("cls"));
        Objects.requireNonNull(primaryKey, ErrorUtils.getNPXMessageSupplier("primaryKey"));
        Objects.requireNonNull(descriptor, ErrorUtils.getNPXMessageSupplier("descriptor"));

        final T result = readObjectInternal(cls, primaryKey, descriptor);
        if (result != null) {
            final EntityTypeImpl<?> et = entityType(cls);
            et.getLifecycleListenerManager().invokePostLoadCallbacks(result);
        }
        return result;
    }

    private <T> T readObjectInternal(Class<T> cls, Object identifier, Descriptor descriptor) {
        assert cls != null;
        assert identifier != null;
        assert descriptor != null;
        // First try to find the object among new uncommitted objects
        Object result = newObjectsKeyToClone.get(identifier);
        if (result != null && (isInRepository(descriptor, result))) {
            // The result can be returned, since it is already registered in this UOW
            return cls.cast(result);
        }
        // Object is already managed
        result = keysToClones.get(identifier);
        if (result != null) {
            if (!cls.isAssignableFrom(result.getClass())) {
                throw individualAlreadyManaged(identifier);
            }
            if (isInRepository(descriptor, result) && !getDeletedObjects().containsKey(result)) {
                return cls.cast(result);
            }
        }
        final URI idUri = EntityPropertiesUtils.getValueAsURI(identifier);
        result = storage.find(new LoadingParameters<>(cls, idUri, descriptor));

        if (result == null) {
            return null;
        }
        final Object clone = registerExistingObject(result, descriptor);
        checkForCollections(clone);
        return cls.cast(clone);
    }

    private static OWLEntityExistsException individualAlreadyManaged(Object identifier) {
        return new OWLEntityExistsException(
                "An entity with URI " + identifier + " is already present in the current persistence context.");
    }

    /**
     * This method calculates the changes that were to the registered entities and adds these changes into the given
     * change set for future commit to the ontology.
     */
    private void calculateChanges() {
        final UnitOfWorkChangeSet changeSet = getUowChangeSet();
        if (hasNew) {
            calculateNewObjects(changeSet);
        }
        if (hasDeleted) {
            calculateDeletedObjects(changeSet);
        }
    }

    /**
     * Create object change sets for the new objects and adds them into our UnitOfWorkChangeSet.
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

    @Override
    public void clear() {
        detachAllManagedInstances();
        cloneMapping.clear();
        cloneToOriginals.clear();
        keysToClones.clear();
        this.deletedObjects = null;
        this.newObjectsCloneToOriginal = null;
        this.newObjectsOriginalToClone = null;
        this.newObjectsKeyToClone.clear();
        this.hasChanges = false;
        this.hasDeleted = false;
        this.hasNew = false;
    }

    private void detachAllManagedInstances() {
        cloneMapping.keySet().forEach(this::unregisterObjectFromPersistenceContext);
    }

    @Override
    public boolean contains(Object entity) {
        Objects.requireNonNull(entity);

        return isObjectManaged(entity);
    }

    @Override
    public void commit() {
        LOG.trace("UnitOfWork commit started.");
        if (!isActive()) {
            throw new IllegalStateException("Cannot commit inactive Unit of Work!");
        }
        this.inCommit = true;
        commitUnitOfWork();
        LOG.trace("UnitOfWork commit finished.");
    }

    @Override
    public void rollback() {
        LOG.trace("UnitOfWork rollback started.");
        if (!isActive()) {
            throw new IllegalStateException("Cannot rollback inactive Unit of Work!");
        }
        storage.rollback();
        clear();
    }

    /**
     * Commit this Unit of Work.
     */
    private void commitUnitOfWork() {
        commitToOntology();
        mergeChangesIntoParent();
        postCommit();
    }

    /**
     * Clean up after the commit.
     */
    private void postCommit() {
        // Remove indirect collections from clones
        cloneMapping.keySet().forEach(this::removeIndirectCollections);
        getNewObjectsCloneToOriginal().clear();
        getNewObjectsOriginalToClone().clear();
        newObjectsKeyToClone.clear();
        getDeletedObjects().clear();
        cloneToOriginals.clear();
        cloneMapping.clear();
        keysToClones.clear();
        this.hasChanges = false;
        this.hasDeleted = false;
        this.hasNew = false;
        this.inCommit = false;
        cloneBuilder.reset();
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
    private void commitToOntology() {
        boolean changes = this.hasNew || this.hasChanges || this.hasDeleted;
        if (changes) {
            calculateChanges();
        }
        validateIntegrityConstraints();
        storageCommit();
    }

    private void validateIntegrityConstraints() {
        if (uowChangeSet == null) {
            return;
        }
        final IntegrityConstraintsValidator validator = IntegrityConstraintsValidator.getValidator();
        for (ObjectChangeSet changeSet : uowChangeSet.getNewObjects()) {
            validator.validate(changeSet.getCloneObject(),
                    entityType((Class<Object>) changeSet.getObjectClass()), false);
        }
        uowChangeSet.getExistingObjectsChanges().forEach(changeSet -> validator.validate(changeSet, getMetamodel()));
    }

    private static Map<Object, Object> createMap() {
        return new IdentityHashMap<>();
    }

    /**
     * Gets current state of the specified entity.
     * <p>
     * Note that since no repository is specified we can only determine if the entity is managed or removed. Therefore
     * if the case is different this method returns State#NOT_MANAGED.
     *
     * @param entity The entity to check
     * @return State of the entity
     */
    public State getState(Object entity) {
        Objects.requireNonNull(entity);

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
     * Checks the state of the specified entity with regards to the specified repository.
     *
     * @param entity     Object
     * @param descriptor Entity descriptor
     * @return The state of the specified entity
     */
    public State getState(Object entity, Descriptor descriptor) {
        Objects.requireNonNull(entity, ErrorUtils.getNPXMessageSupplier("entity"));
        Objects.requireNonNull(descriptor, ErrorUtils.getNPXMessageSupplier("descriptor"));

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
     * Tries to find the original object for the given clone. It searches the existing objects, new objects and deleted
     * objects.
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
     * Gets managed original with the specified identifier or {@code null} if there is none matching.
     * <p>
     * Descriptor is used to check repository context validity.
     *
     * @param cls        Return type of the original
     * @param identifier Instance identifier
     * @param descriptor Repository descriptor
     * @return Original object managed by this UoW or {@code null} if this UoW doesn't contain a matching instance
     */
    public <T> T getManagedOriginal(Class<T> cls, Object identifier, Descriptor descriptor) {
        if (!keysToClones.containsKey(identifier)) {
            return null;
        }
        final Object clone = keysToClones.get(identifier);
        if (!cls.isAssignableFrom(clone.getClass())) {
            return null;
        }
        if (!isInRepository(descriptor, clone)) {
            return null;
        }
        return cls.cast(cloneToOriginals.get(clone));
    }

    /**
     * Check if this UnitOfWork contains this original entity. This method is used by the CloneBuilder so it does not
     * have to clone already managed referenced objects.
     *
     * @param entity The original entity.
     * @return True if the original is managed in this UnitOfWork.
     */
    boolean containsOriginal(Object entity) {
        return entity != null && cloneToOriginals.containsValue(entity);
    }

    /**
     * Finds clone of the specified original.
     *
     * @param original The original object whose clone we are looking for
     * @return The clone or null, if there is none
     */
    public Object getCloneForOriginal(Object original) {
        for (Entry<Object, Object> entry : cloneToOriginals.entrySet()) {
            // We use IdentityMap, so we can use ==
            if (entry.getValue() == original) {
                return entry.getKey();
            }
        }
        return null;
    }

    public boolean hasChanges() {
        return hasChanges || hasDeleted || hasNew;
    }

    void setHasChanges() {
        this.hasChanges = true;
    }

    Map<Object, Object> getDeletedObjects() {
        if (deletedObjects == null) {
            this.deletedObjects = createMap();
        }
        return deletedObjects;
    }

    Map<Object, Object> getNewObjectsCloneToOriginal() {
        if (newObjectsCloneToOriginal == null) {
            this.newObjectsCloneToOriginal = createMap();
        }
        return newObjectsCloneToOriginal;
    }

    private Map<Object, Object> getNewObjectsOriginalToClone() {
        if (newObjectsOriginalToClone == null) {
            this.newObjectsOriginalToClone = createMap();
        }
        return newObjectsOriginalToClone;
    }

    @Override
    public CacheManager getLiveObjectCache() {
        return parent.getLiveObjectCache();
    }

    public UnitOfWorkChangeSet getUowChangeSet() {
        if (uowChangeSet == null) {
            this.uowChangeSet = ChangeSetFactory.createUoWChangeSet();
        }
        return uowChangeSet;
    }

    @Override
    public boolean isActive() {
        return this.isActive;
    }

    /**
     * Returns true if the given clone represents a newly created object. Otherwise returns false.
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
    @Override
    public boolean isObjectManaged(Object entity) {
        Objects.requireNonNull(entity);

        return cloneMapping.containsKey(entity) && !getDeletedObjects().containsKey(entity);
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
        final EntityTypeImpl<?> et = entityType(entity.getClass());
        et.getLifecycleListenerManager().invokePreUpdateCallbacks(entity);
        storage.merge(entity, f, descriptor);
        createChangeRecord(entity, f, descriptor);
        setHasChanges();
        setIndirectCollectionIfPresent(entity, f);
        et.getLifecycleListenerManager().invokePostUpdateCallbacks(entity);
    }

    private void createChangeRecord(Object clone, Field field, Descriptor descriptor) {
        final Object orig = getOriginal(clone);
        if (orig == null) {
            return;
        }
        final ChangeRecord record = new ChangeRecordImpl(field.getName(),
                EntityPropertiesUtils.getFieldValue(field, clone));
        registerChangeRecord(clone, orig, descriptor, record);
    }

    private void registerChangeRecord(Object clone, Object orig, Descriptor descriptor, ChangeRecord record) {
        ObjectChangeSet chSet = getUowChangeSet().getExistingObjectChanges(orig);
        if (chSet == null) {
            chSet = ChangeSetFactory.createObjectChangeSet(orig, clone, descriptor);
            getUowChangeSet().addObjectChangeSet(chSet);
        }
        chSet.addChangeRecord(record);
    }

    /**
     * Merge the changes from this Unit of Work's change set into the server session.
     */
    private void mergeChangesIntoParent() {
        if (hasChanges()) {
            mergeManager.mergeChangesFromChangeSet(getUowChangeSet());
        }
    }

    @Override
    public <T> T mergeDetached(T entity, Descriptor descriptor) {
        Objects.requireNonNull(entity, ErrorUtils.getNPXMessageSupplier("entity"));
        Objects.requireNonNull(descriptor, ErrorUtils.getNPXMessageSupplier("descriptor"));

        final Object id = getIdentifier(entity);
        if (!storage.contains(id, entity.getClass(), descriptor)) {
            registerNewObject(entity, descriptor);
            return entity;
        } else {
            if (isIndividualManaged(id, entity) && !isSameType(id, entity)) {
                throw individualAlreadyManaged(id);
            }
            return mergeDetachedInternal(entity, descriptor);
        }
    }

    private boolean isSameType(Object id, Object entity) {
        final Class<?> mergedType = entity.getClass();
        final Object managed = keysToClones.containsKey(id) ? keysToClones.get(id) : newObjectsKeyToClone.get(id);
        return managed != null && managed.getClass().isAssignableFrom(mergedType);
    }

    private <T> T mergeDetachedInternal(T entity, Descriptor descriptor) {
        assert entity != null;
        final Object iri = getIdentifier(entity);
        final EntityTypeImpl<T> et = (EntityTypeImpl<T>) entityType(entity.getClass());
        final URI idUri = EntityPropertiesUtils.getValueAsURI(iri);
        T original = storage.find(new LoadingParameters<>(et.getJavaType(), idUri, descriptor, true));
        assert original != null;

        final Object clone = registerExistingObject(original, descriptor);
        try {
            // Merge only the changed attributes
            final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(clone, entity, descriptor);
            changeManager.calculateChanges(chSet);
            if (chSet.hasChanges()) {
                et.getLifecycleListenerManager().invokePreUpdateCallbacks(clone);
            }
            final DetachedInstanceMerger merger = new DetachedInstanceMerger(this);
            merger.mergeChangesFromDetachedToManagedInstance(chSet, descriptor);
            for (ChangeRecord record : chSet.getChanges().values()) {
                final Field field = et.getFieldSpecification(record.getAttributeName()).getJavaField();
                storage.merge(clone, field, descriptor);
            }
            if (chSet.hasChanges()) {
                et.getLifecycleListenerManager().invokePostUpdateCallbacks(clone);
            }
            getUowChangeSet().addObjectChangeSet(chSet);
        } catch (OWLEntityExistsException e) {
            unregisterObject(clone);
            throw e;
        } catch (IllegalAccessException e) {
            throw new OWLPersistenceException(e);
        }
        if (cacheManager.contains(et.getJavaType(), iri, descriptor.getContext())) {
            cacheManager.evict(et.getJavaType(), iri, descriptor.getContext());
        }
        setHasChanges();
        checkForCollections(clone);
        return et.getJavaType().cast(clone);
    }

    @Override
    void registerEntityWithPersistenceContext(Object entity, UnitOfWorkImpl uow) {
        parent.registerEntityWithPersistenceContext(entity, uow);
    }

    @Override
    void deregisterEntityFromPersistenceContext(Object entity, UnitOfWork uow) {
        parent.deregisterEntityFromPersistenceContext(entity, uow);
    }

    @Override
    void releasePersistenceContext(UnitOfWork uow) {
        parent.releasePersistenceContext(uow);
    }

    @Override
    public NamedQueryManager getNamedQueryManager() {
        return parent.getNamedQueryManager();
    }

    @Override
    public Object registerExistingObject(Object entity, Descriptor descriptor) {
        if (entity == null) {
            return null;
        }
        if (cloneToOriginals.containsValue(entity)) {
            return getCloneForOriginal(entity);
        }
        Object clone = cloneBuilder.buildClone(entity, descriptor);
        assert clone != null;
        registerClone(clone, entity, descriptor);
        return clone;
    }

    private void registerClone(Object clone, Object original, Descriptor descriptor) {
        cloneMapping.put(clone, clone);
        cloneToOriginals.put(clone, original);
        final Object identifier = EntityPropertiesUtils.getPrimaryKey(clone, getMetamodel());
        keysToClones.put(identifier, clone);
        registerEntityWithPersistenceContext(clone, this);
        registerEntityWithOntologyContext(descriptor, clone);
    }

    /**
     * Release this Unit of Work. Releasing an active Unit of Work with uncommitted changes causes all pending changes
     * to be discarded.
     */
    @Override
    public void release() {
        clear();
        storage.close();
        releasePersistenceContext(this);
        this.isActive = false;
        LOG.debug("UnitOfWork released.");
    }

    @Override
    public <T> void revertObject(T object) {
        Objects.requireNonNull(object);

        if (!isObjectManaged(object) && !getDeletedObjects().containsKey(object)) {
            throw new IllegalArgumentException(
                    "The specified entity " + object + " is not managed by this persistence context.");
        }
        final Descriptor descriptor = getDescriptor(object);
        if (descriptor == null) {
            throw new IllegalArgumentException("Unable to find entity " + object + " in this persistence context.");
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
                mergeManager.mergeChangesOnObject(chSet);
            }
        } catch (IllegalAccessException | IllegalArgumentException e) {
            throw new OWLPersistenceException(e);
        }
        entityType(object.getClass()).getLifecycleListenerManager().invokePostLoadCallbacks(object);
    }

    @Override
    public void registerNewObject(Object entity, Descriptor descriptor) {
        Objects.requireNonNull(entity, ErrorUtils.getNPXMessageSupplier("entity"));
        Objects.requireNonNull(descriptor, ErrorUtils.getNPXMessageSupplier("descriptor"));

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
        final EntityTypeImpl<?> eType = entityType(entity.getClass());
        eType.getLifecycleListenerManager().invokePrePersistCallbacks(entity);
        Object id = getIdentifier(entity);
        if (id == null) {
            EntityPropertiesUtils.verifyIdentifierIsGenerated(entity, eType);
        }
        verifyCanPersist(id, entity, eType, descriptor);
        storage.persist(id, entity, descriptor);
        if (id == null) {
            // If the ID was null, extract it from the entity. It is present now
            id = getIdentifier(entity);
        }
        assert id != null;
        // Original is null until commit
        cloneMapping.put(entity, entity);
        getNewObjectsCloneToOriginal().put(entity, null);
        registerEntityWithPersistenceContext(entity, this);
        registerEntityWithOntologyContext(descriptor, entity);
        newObjectsKeyToClone.put(id, entity);
        checkForCollections(entity);
        this.hasNew = true;
        eType.getLifecycleListenerManager().invokePostPersistCallbacks(entity);
    }

    private void verifyCanPersist(Object id, Object instance, EntityType<?> et, Descriptor descriptor) {
        if (isIndividualManaged(id, instance) && !instance.getClass().isEnum()) {
            throw individualAlreadyManaged(id);
        }
        if (storage.contains(id, instance.getClass(), descriptor)) {
            throw new OWLEntityExistsException(
                    "Individual " + id + " of type " + et.getIRI() + " already exists in storage.");
        }
    }

    private boolean isIndividualManaged(Object identifier, Object entity) {
        return keysToClones.containsKey(identifier) ||
                newObjectsKeyToClone.containsKey(identifier) && !cloneMapping.containsKey(entity);
    }

    /**
     * Remove the specified entity from the ontology.
     *
     * @param entity Managed entity to delete
     */
    @Override
    public void removeObject(Object entity) {
        assert entity != null;
        if (!isObjectManaged(entity)) {
            throw new IllegalArgumentException(
                    "Cannot remove entity which is not managed in the current persistence context.");
        }
        final EntityTypeImpl<?> et = entityType(entity.getClass());
        et.getLifecycleListenerManager().invokePreRemoveCallbacks(entity);
        final Object primaryKey = getIdentifier(entity);
        final Descriptor descriptor = getDescriptor(entity);

        if (hasNew && getNewObjectsCloneToOriginal().containsKey(entity)) {
            unregisterObject(entity);
            newObjectsKeyToClone.remove(primaryKey);
        } else {
            getDeletedObjects().put(entity, entity);
            this.hasDeleted = true;
        }
        storage.remove(primaryKey, et.getJavaType(), descriptor);
        et.getLifecycleListenerManager().invokePostRemoveCallbacks(entity);
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
        final Object original = cloneToOriginals.remove(object);
        keysToClones.remove(EntityPropertiesUtils.getPrimaryKey(object, getMetamodel()));

        getDeletedObjects().remove(object);
        if (hasNew) {
            Object newOriginal = getNewObjectsCloneToOriginal().remove(object);
            if (newOriginal != null) {
                getNewObjectsOriginalToClone().remove(newOriginal);
            }
        }
        if (original != null) {
            cloneBuilder.removeVisited(original, repoMap.getEntityDescriptor(object));
        }
        unregisterObjectFromPersistenceContext(object);
    }

    private void unregisterObjectFromPersistenceContext(Object object) {
        removeIndirectCollections(object);
        deregisterEntityFromPersistenceContext(object, this);
        unregisterEntityFromOntologyContext(object);
    }

    @Override
    public boolean shouldReleaseAfterCommit() {
        return shouldReleaseAfterCommit;
    }

    public void setShouldClearAfterCommit(boolean shouldClearCache) {
        this.shouldClearCacheAfterCommit = shouldClearCache;
    }

    public void setEntityManager(AbstractEntityManager entityManager) {
        this.entityManager = entityManager;
    }

    @Override
    public void writeUncommittedChanges() {
        if (!hasChanges()) {
            return;
        }
        commitUnitOfWork();
    }

    @Override
    public MetamodelImpl getMetamodel() {
        return parent.getMetamodel();
    }

    private <T> EntityTypeImpl<T> entityType(Class<T> cls) {
        return getMetamodel().entity(cls);
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
     *
     * @return Whether this UoW is in the commit phase
     */
    public boolean isInCommit() {
        return inCommit;
    }

    @Override
    public <T> void loadEntityField(T entity, Field field) {
        Objects.requireNonNull(entity, ErrorUtils.getNPXMessageSupplier("entity"));
        Objects.requireNonNull(field, ErrorUtils.getNPXMessageSupplier("field"));

        if (EntityPropertiesUtils.getFieldValue(field, entity) != null) {
            return;
        }
        final Descriptor entityDescriptor = getDescriptor(entity);
        if (entityDescriptor == null) {
            throw new OWLPersistenceException(
                    "Unable to find repository identifier for entity " + entity
                            + ". Is it managed by this UoW?");
        }
        storage.loadFieldValue(entity, field, entityDescriptor);
        final Object orig = EntityPropertiesUtils.getFieldValue(field, entity);
        final Object entityOriginal = getOriginal(entity);
        if (entityOriginal != null) {
            EntityPropertiesUtils.setFieldValue(field, entityOriginal, orig);
        }
        final Descriptor fieldDescriptor = getFieldDescriptor(entity, field, entityDescriptor);
        final Object clone = cloneLoadedFieldValue(entity, field, fieldDescriptor, orig);
        EntityPropertiesUtils.setFieldValue(field, entity, clone);
    }

    private <T> Descriptor getFieldDescriptor(T entity, Field field, Descriptor entityDescriptor) {
        final EntityType<?> et = entityType(entity.getClass());
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
        Objects.requireNonNull(toRemove, ErrorUtils.getNPXMessageSupplier("toRemove"));

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
    public QueryImpl createNativeQuery(String sparql) {
        return queryFactory.createNativeQuery(sparql);
    }

    @Override
    public <T> TypedQueryImpl<T> createNativeQuery(String sparql, Class<T> resultClass) {
        return queryFactory.createNativeQuery(sparql, resultClass);
    }

    @Override
    public QueryImpl createQuery(String query) {
        return queryFactory.createQuery(query);
    }

    @Override
    public <T> TypedQueryImpl<T> createQuery(String query, Class<T> resultClass) {
        return queryFactory.createQuery(query, resultClass);
    }

    @Override
    public QueryImpl createNamedQuery(String name) {
        return queryFactory.createNamedQuery(name);
    }

    @Override
    public <T> TypedQueryImpl<T> createNamedQuery(String name, Class<T> resultClass) {
        return queryFactory.createNamedQuery(name, resultClass);
    }

    /**
     * Check if the specified entity contains a collection. If so, replace it with its indirect representation so that
     * changes in that collection can be tracked.
     *
     * @param entity The entity to check
     */
    private void checkForCollections(Object entity) {
        assert entity != null;
        final EntityType<?> et = entityType(entity.getClass());
        for (FieldSpecification<?, ?> fieldSpec : et.getFieldSpecifications()) {
            setIndirectCollectionIfPresent(entity, fieldSpec.getJavaField());
        }
    }

    /**
     * Create and set indirect collection on the specified entity field.
     * <p>
     * If the specified field is of Collection type and it is not already an indirect collection, create new one and set
     * it as the value of the specified field on the specified entity.
     *
     * @param entity The entity collection will be set on
     * @param field  The field to set
     * @throws IllegalArgumentException Reflection
     */
    private void setIndirectCollectionIfPresent(Object entity, Field field) {
        assert entity != null;
        assert field != null;

        final Object value = EntityPropertiesUtils.getFieldValue(field, entity);
        if (value == null || value instanceof IndirectCollection) {
            return;
        }
        if (value instanceof Collection || value instanceof Map) {
            EntityPropertiesUtils.setFieldValue(field, entity, createIndirectCollection(value, entity, field));
        }
    }

    /**
     * Creates an indirect collection, which wraps the specified collection instance and propagates changes to the
     * persistence context.
     *
     * @param collection Collection to be proxied
     * @param owner      Collection owner instance
     * @param field      Field filled with the collection
     * @return Indirect collection
     */
    public IndirectCollection<?> createIndirectCollection(Object collection, Object owner, Field field) {
        return collectionFactory.createIndirectCollection(collection, owner, field);
    }

    /**
     * Remove indirect collection implementations from the specified entity (if present).
     *
     * @param entity The entity to remove indirect collections from
     */
    private void removeIndirectCollections(Object entity) {
        Field[] fields = entity.getClass().getDeclaredFields();
        for (Field f : fields) {
            final Object ob = EntityPropertiesUtils.getFieldValue(f, entity);
            if (ob == null) {
                continue;
            }
            if (ob instanceof IndirectCollection) {
                IndirectCollection<?> indCol = (IndirectCollection<?>) ob;
                EntityPropertiesUtils.setFieldValue(f, entity, indCol.getReferencedCollection());
            }
        }
    }

    void putObjectIntoCache(Object primaryKey, Object entity, URI context) {
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

    @Override
    public Configuration getConfiguration() {
        return entityManager.getConfiguration();
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        }
        return storage.unwrap(cls);
    }
}
