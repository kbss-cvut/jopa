/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.adapters.IndirectWrapper;
import cz.cvut.kbss.jopa.exceptions.EntityNotFoundException;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.AbstractEntityManager;
import cz.cvut.kbss.jopa.model.EntityManagerImpl.State;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.Manageable;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.lifecycle.PostLoadInvoker;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;
import cz.cvut.kbss.jopa.query.criteria.CriteriaBuilderImpl;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryFactory;
import cz.cvut.kbss.jopa.sessions.change.ChangeManagerImpl;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.descriptor.InstanceDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.InstanceDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.validator.AttributeModificationValidator;
import cz.cvut.kbss.jopa.sessions.validator.InferredAttributeChangeValidator;
import cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.MetamodelUtils;
import cz.cvut.kbss.jopa.utils.Wrapper;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;

import static cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException.individualAlreadyManaged;
import static cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator.getValidator;
import static cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator.isNotInferred;
import static cz.cvut.kbss.jopa.utils.EntityPropertiesUtils.getValueAsURI;

public class UnitOfWorkImpl extends AbstractSession implements UnitOfWork, ConfigurationHolder, Wrapper {

    // Read-only!!! It is just the keyset of cloneToOriginals
    private final Set<Object> cloneMapping;
    private final Map<Object, Object> cloneToOriginals;
    private final Map<Object, Object> keysToClones = new HashMap<>();
    private final Map<Object, Object> deletedObjects;
    private final Map<Object, Object> newObjectsCloneToOriginal;
    private final Map<Object, Object> newObjectsKeyToClone = new HashMap<>();
    private final Map<Object, InstanceDescriptor> instanceDescriptors;
    private RepositoryMap repoMap;

    private boolean hasChanges;
    private boolean hasNew;
    private boolean hasDeleted;
    private boolean shouldReleaseAfterCommit;
    private boolean shouldClearCacheAfterCommit;

    private boolean isActive;
    private boolean inCommit;

    private UnitOfWorkChangeSet uowChangeSet = ChangeSetFactory.createUoWChangeSet();

    private final AbstractSession parent;
    private AbstractEntityManager entityManager;
    private final ConnectionWrapper storage;

    private final MergeManager mergeManager;
    private final CloneBuilder cloneBuilder;
    private final ChangeManager changeManager;
    private final SparqlQueryFactory queryFactory;
    private final CriteriaBuilder criteriaFactory;
    private final IndirectWrapperHelper indirectWrapperHelper;
    private final InferredAttributeChangeValidator inferredAttributeChangeValidator;
    /**
     * This is a shortcut for the second level cache.
     */
    private final CacheManager cacheManager;

    public UnitOfWorkImpl(AbstractSession parent) {
        super(parent.getConfiguration());
        this.parent = Objects.requireNonNull(parent);
        this.cloneToOriginals = createMap();
        this.cloneMapping = cloneToOriginals.keySet();
        this.deletedObjects = createMap();
        this.newObjectsCloneToOriginal = createMap();
        this.instanceDescriptors = new IdentityHashMap<>();
        this.repoMap = new RepositoryMap();
        this.cloneBuilder = new CloneBuilderImpl(this);
        this.indirectWrapperHelper = new IndirectWrapperHelper(this);
        this.cacheManager = parent.getLiveObjectCache();
        this.storage = acquireConnection();
        this.queryFactory = new SparqlQueryFactory(this, storage);
        this.criteriaFactory = new CriteriaBuilderImpl(this);
        this.mergeManager = new MergeManagerImpl(this);
        this.changeManager = new ChangeManagerImpl(this);
        this.inferredAttributeChangeValidator = new InferredAttributeChangeValidator(storage);
        this.isActive = true;
    }

    CloneBuilder getCloneBuilder() {
        return cloneBuilder;
    }

    @Override
    public UnitOfWork acquireUnitOfWork() {
        throw new UnsupportedOperationException("Nested UoWs are not supported.");
    }

    @Override
    protected ConnectionWrapper acquireConnection() {
        final ConnectionWrapper conn = parent.acquireConnection();
        conn.setUnitOfWork(this);
        return conn;
    }

    @Override
    public <T> T readObject(Class<T> cls, Object identifier, Descriptor descriptor) {
        Objects.requireNonNull(cls);
        Objects.requireNonNull(identifier);
        Objects.requireNonNull(descriptor);

        return readObjectInternal(cls, identifier, descriptor);
    }

    private <T> T readObjectInternal(Class<T> cls, Object identifier, Descriptor descriptor) {
        assert cls != null;
        assert identifier != null;
        assert descriptor != null;
        T result = readManagedObject(cls, identifier, descriptor);
        if (result != null) {
            return result;
        }
        result = storage.find(new LoadingParameters<>(cls, getValueAsURI(identifier), descriptor));

        if (result == null) {
            return null;
        }
        final Object clone = registerExistingObject(result, descriptor, Collections.singletonList(new PostLoadInvoker(getMetamodel())));
        checkForIndirectObjects(clone);
        return cls.cast(clone);
    }

    private <T> T readManagedObject(Class<T> cls, Object identifier, Descriptor descriptor) {
        // First try to find the object among new uncommitted objects
        Object result = newObjectsKeyToClone.get(identifier);
        if (result != null && (isInRepository(descriptor, result))) {
            // The result can be returned, since it is already registered in this UOW
            return cls.cast(result);
        }
        // Object is already managed
        return getManagedClone(cls, identifier, descriptor);
    }

    private <T> T getManagedClone(Class<T> cls, Object identifier, Descriptor descriptor) {
        if (!keysToClones.containsKey(identifier)) {
            return null;
        }
        final Object clone = keysToClones.get(identifier);
        if (!cls.isAssignableFrom(clone.getClass())) {
            throw individualAlreadyManaged(identifier);
        }
        return isInRepository(descriptor, clone) && !deletedObjects.containsKey(clone) ? cls.cast(clone) : null;
    }

    /**
     * Reads an object but does not register it with this persistence context.
     * <p>
     * Useful when the caller knows the object will be registered eventually by another routine.
     *
     * @param cls        Expected result class
     * @param identifier Object identifier
     * @param descriptor Entity descriptor
     * @return The retrieved object or {@code null} if there is no object with the specified identifier in the specified
     * repository
     */
    public <T> T readObjectWithoutRegistration(Class<T> cls, Object identifier, Descriptor descriptor) {
        Objects.requireNonNull(cls);
        Objects.requireNonNull(identifier);
        Objects.requireNonNull(descriptor);

        T result = readManagedObject(cls, identifier, descriptor);
        if (result != null) {
            return result;
        }
        return storage.find(new LoadingParameters<>(cls, getValueAsURI(identifier), descriptor));
    }

    @Override
    public <T> T getReference(Class<T> cls, Object identifier, Descriptor descriptor) {
        Objects.requireNonNull(cls);
        Objects.requireNonNull(identifier);
        Objects.requireNonNull(descriptor);

        final T managedResult = readManagedObject(cls, identifier, descriptor);
        if (managedResult != null) {
            return managedResult;
        }
        final T result = storage.getReference(new LoadingParameters<>(cls, getValueAsURI(identifier), descriptor));
        if (result == null) {
            return null;
        }
        final T clone = (T) cloneBuilder.buildReferenceClone(result, new CloneConfiguration(descriptor, true));
        instanceDescriptors.put(clone, InstanceDescriptorFactory.createNotLoaded(result, entityType(cls)));
        registerEntityWithPersistenceContext(clone);
        registerEntityWithOntologyContext(clone, descriptor);
        if (getLiveObjectCache().contains(cls, identifier, descriptor)) {
            cloneToOriginals.put(clone, getLiveObjectCache().get(cls, identifier, descriptor));
        } else {
            cloneToOriginals.put(clone, null);
        }
        keysToClones.put(identifier, clone);
        return clone;
    }

    /**
     * This method calculates the changes that were to the registered entities and adds these changes into the given
     * change set for future commit to the ontology.
     */
    private void calculateChanges() {
        if (hasNew) {
            calculateNewObjects(uowChangeSet);
        }
        if (hasDeleted) {
            calculateDeletedObjects(uowChangeSet);
        }
    }

    /**
     * Create object change sets for the new objects and adds them into our UnitOfWorkChangeSet.
     */
    private void calculateNewObjects(UnitOfWorkChangeSet changeSet) {
        for (Object clone : newObjectsCloneToOriginal.keySet()) {
            final Descriptor c = getDescriptor(clone);
            Object original = newObjectsCloneToOriginal.computeIfAbsent(clone, key -> cloneBuilder.buildClone(key, new CloneConfiguration(c, false)));
            if (original == null) {
                throw new OWLPersistenceException("Error while calculating changes for new objects. Original not found.");
            }
            newObjectsCloneToOriginal.put(clone, original);
            changeSet.addNewObjectChangeSet(ChangeSetFactory.createObjectChangeSet(original, clone, c));
        }
    }

    private void calculateDeletedObjects(final UnitOfWorkChangeSet changeSet) {
        for (Object clone : deletedObjects.keySet()) {
            Descriptor descriptor = getDescriptor(clone);
            changeSet.addDeletedObjectChangeSet(ChangeSetFactory.createDeleteObjectChangeSet(clone, descriptor));
            changeSet.cancelObjectChanges(getOriginal(clone));
        }
    }

    @Override
    public void clear() {
        detachAllManagedInstances();
        cloneToOriginals.clear();
        keysToClones.clear();
        deletedObjects.clear();
        newObjectsCloneToOriginal.clear();
        newObjectsKeyToClone.clear();
        instanceDescriptors.clear();
        this.hasChanges = false;
        this.hasDeleted = false;
        this.hasNew = false;
        cloneBuilder.reset();
        this.repoMap = new RepositoryMap();
        repoMap.initDescriptors();
        this.uowChangeSet = ChangeSetFactory.createUoWChangeSet();
    }

    private void detachAllManagedInstances() {
        cloneMapping.forEach(instance -> {
            removeIndirectWrappers(instance);
            deregisterEntityFromPersistenceContext(instance);
        });
        newObjectsCloneToOriginal.keySet().forEach(this::removeIndirectWrappers);
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
        final boolean changes = hasChanges();
        clear();
        this.inCommit = false;
        if (changes) {
            if (shouldClearCacheAfterCommit) {
                cacheManager.evictAll();
                this.shouldReleaseAfterCommit = true;
            } else {
                cacheManager.evictInferredObjects();
            }
        }
    }

    /**
     * If there are any changes, commit them to the ontology.
     */
    private void commitToOntology() {
        if (this.hasNew || this.hasChanges || this.hasDeleted) {
            persistNewObjects();
            calculateChanges();
        }
        validateIntegrityConstraints();
        storageCommit();
    }

    private void persistNewObjects() {
        if (hasNew) {
            newObjectsKeyToClone.forEach((id, entity) -> {
                final Descriptor descriptor = getDescriptor(entity);
                storage.persist(id, entity, descriptor);
                final IdentifiableEntityType<?> et = entityType(entity.getClass());
                et.getLifecycleListenerManager().invokePostPersistCallbacks(entity);
            });
        }
    }

    private void validateIntegrityConstraints() {
        final IntegrityConstraintsValidator validator = getValidator();
        for (ObjectChangeSet changeSet : uowChangeSet.getNewObjects()) {
            validator.validate(changeSet.getCloneObject(), entityType((Class<Object>) changeSet.getObjectClass()), isNotInferred());
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

        if (deletedObjects.containsKey(entity)) {
            return State.REMOVED;
        } else if (newObjectsCloneToOriginal.containsKey(entity)) {
            return State.MANAGED_NEW;
        } else if (cloneMapping.contains(entity)) {
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
        Objects.requireNonNull(entity);
        Objects.requireNonNull(descriptor);

        if (deletedObjects.containsKey(entity)) {
            return State.REMOVED;
        } else if (newObjectsCloneToOriginal.containsKey(entity) && isInRepository(descriptor, entity)) {
            return State.MANAGED_NEW;
        } else if (cloneMapping.contains(entity) && isInRepository(descriptor, entity)) {
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
        return cloneToOriginals.containsKey(clone) ? cloneToOriginals.get(clone) : newObjectsCloneToOriginal.get(clone);
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
        final T clone = getManagedClone(cls, identifier, descriptor);
        return clone != null ? cls.cast(cloneToOriginals.get(clone)) : null;
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

    @Override
    public CacheManager getLiveObjectCache() {
        return parent.getLiveObjectCache();
    }

    UnitOfWorkChangeSet getUowChangeSet() {
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
        return clone != null && newObjectsCloneToOriginal.containsKey(clone);
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

        return cloneMapping.contains(entity) && !deletedObjects.containsKey(entity) || newObjectsCloneToOriginal.containsKey(entity);
    }

    /**
     * Persists changed value of the specified field.
     *
     * @param entity Entity with changes (the clone)
     * @param f      The field whose value has changed
     * @throws IllegalStateException If this UoW is not in transaction
     * @see #attributeChanged(Object, FieldSpecification)
     */
    public void attributeChanged(Object entity, Field f) {
        final IdentifiableEntityType<Object> et = entityType((Class<Object>) entity.getClass());
        final FieldSpecification<Object, ?> fieldSpec = et.getFieldSpecification(f.getName());
        attributeChanged(entity, fieldSpec);
    }

    /**
     * Persists changed value of the specified field.
     *
     * @param entity Entity with changes (the clone)
     * @param fieldSpec Metamodel element representing the attribute that changed
     * @throws IllegalStateException If this UoW is not in transaction
     */
    public void attributeChanged(Object entity, FieldSpecification<?, ?> fieldSpec) {
        if (!isInTransaction()) {
            throw new IllegalStateException("This unit of work is not in a transaction.");
        }
        final Descriptor descriptor = getDescriptor(entity);
        final IdentifiableEntityType<Object> et = entityType((Class<Object>) entity.getClass());
        final Object original = getOriginal(entity);
        if (fieldSpec.isInferred() && original != null) {
            inferredAttributeChangeValidator.validateChange(entity, getOriginal(entity), (FieldSpecification<? super Object, ?>) fieldSpec, descriptor);
        }
        et.getLifecycleListenerManager().invokePreUpdateCallbacks(entity);
        storage.merge(entity, (FieldSpecification<? super Object, ?>) fieldSpec, descriptor);
        createAndRegisterChangeRecord(entity, fieldSpec, descriptor);
        setHasChanges();
        setIndirectObjectIfPresent(entity, fieldSpec.getJavaField());
        et.getLifecycleListenerManager().invokePostUpdateCallbacks(entity);
        instanceDescriptors.get(entity).setLoaded(fieldSpec, LoadState.LOADED);
    }

    private void createAndRegisterChangeRecord(Object clone, FieldSpecification<?, ?> fieldSpec,
                                               Descriptor descriptor) {
        final Object orig = getOriginal(clone);
        if (orig == null) {
            return;
        }
        final ChangeRecord record = new ChangeRecordImpl(fieldSpec, EntityPropertiesUtils.getFieldValue(fieldSpec.getJavaField(), clone));
        preventCachingIfReferenceIsNotLoaded(record);
        registerChangeRecord(clone, orig, descriptor, record);
    }

    private void preventCachingIfReferenceIsNotLoaded(ChangeRecord changeRecord) {
        final Object newValue = changeRecord.getNewValue();
        if (newValue != null && contains(newValue) && isLoaded(newValue) != LoadState.LOADED) {
            changeRecord.preventCaching();
        }
    }

    private void registerChangeRecord(Object clone, Object orig, Descriptor descriptor, ChangeRecord record) {
        ObjectChangeSet chSet = uowChangeSet.getExistingObjectChanges(orig);
        if (chSet == null) {
            chSet = ChangeSetFactory.createObjectChangeSet(orig, clone, descriptor);
            uowChangeSet.addObjectChangeSet(chSet);
        }
        chSet.addChangeRecord(record);
    }

    /**
     * Merge the changes from this Unit of Work's change set into the server session.
     */
    private void mergeChangesIntoParent() {
        if (hasChanges()) {
            mergeManager.mergeChangesFromChangeSet(uowChangeSet);
        }
        evictPossiblyUpdatedReferencesFromCache();
    }

    private void evictPossiblyUpdatedReferencesFromCache() {
        cloneToOriginals.forEach((clone, orig) -> {
            if (orig == null && !deletedObjects.containsKey(clone)) {
                removeObjectFromCache(clone, getDescriptor(clone).getSingleContext().orElse(null));
            }
        });
    }

    @Override
    public <T> T mergeDetached(T entity, Descriptor descriptor) {
        Objects.requireNonNull(entity);
        Objects.requireNonNull(descriptor);

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
        assert managed != null;
        final Class<?> managedType = MetamodelUtils.getEntityClass(managed.getClass());
        return managedType.isAssignableFrom(mergedType);
    }

    private <T> T mergeDetachedInternal(T entity, Descriptor descriptor) {
        assert entity != null;
        final IdentifiableEntityType<T> et = (IdentifiableEntityType<T>) entityType(entity.getClass());
        final URI idUri = EntityPropertiesUtils.getIdentifier(entity, et);

        final T clone = getInstanceForMerge(idUri, et, descriptor);
        try {
            // Merge only the changed attributes
            ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(clone, entity, descriptor);
            // Have to check for inferred attribute changes before the actual merge
            changeManager.calculateChanges(chSet);
            chSet = processInferredValueChanges(chSet);
            if (chSet.hasChanges()) {
                et.getLifecycleListenerManager().invokePreUpdateCallbacks(clone);
                final DetachedInstanceMerger merger = new DetachedInstanceMerger(this);
                merger.mergeChangesFromDetachedToManagedInstance(chSet, descriptor);
                for (ChangeRecord record : chSet.getChanges()) {
                    AttributeModificationValidator.verifyCanModify(record.getAttribute());
                    preventCachingIfReferenceIsNotLoaded(record);
                    storage.merge(clone, (FieldSpecification<? super T, ?>) record.getAttribute(), descriptor);
                }
                et.getLifecycleListenerManager().invokePostUpdateCallbacks(clone);
                uowChangeSet.addObjectChangeSet(copyChangeSet(chSet, getOriginal(clone), clone, descriptor));
            }
        } catch (OWLEntityExistsException e) {
            unregisterObject(clone);
            throw e;
        }
        evictAfterMerge(et, idUri, descriptor);
        setHasChanges();
        checkForIndirectObjects(clone);
        return et.getJavaType().cast(clone);
    }

    private <T> T getInstanceForMerge(URI identifier, EntityType<T> et, Descriptor descriptor) {
        if (keysToClones.containsKey(identifier)) {
            return (T) keysToClones.get(identifier);
        }
        final LoadingParameters<T> params = new LoadingParameters<>(et.getJavaType(), identifier, descriptor, true);
        T original = storage.find(params);
        assert original != null;

        return (T) registerExistingObject(original, descriptor);
    }

    private ObjectChangeSet processInferredValueChanges(ObjectChangeSet changeSet) {
        if (getConfiguration().is(JOPAPersistenceProperties.IGNORE_INFERRED_VALUE_REMOVAL_ON_MERGE)) {
            final ObjectChangeSet copy = ChangeSetFactory.createObjectChangeSet(changeSet.getChangedObject(), changeSet.getCloneObject(), changeSet.getEntityDescriptor());
            changeSet.getChanges().stream().filter(chr -> !(chr.getAttribute().isInferred() &&
                    inferredAttributeChangeValidator.isInferredValueRemoval(changeSet.getCloneObject(), changeSet.getChangedObject(),
                            (FieldSpecification) chr.getAttribute(),
                            changeSet.getEntityDescriptor()))).forEach(copy::addChangeRecord);
            return copy;
        } else {
            changeSet.getChanges().stream().filter(chr -> chr.getAttribute().isInferred()).forEach(
                    chr -> inferredAttributeChangeValidator.validateChange(changeSet.getCloneObject(), changeSet.getChangedObject(),
                            (FieldSpecification) chr.getAttribute(),
                            changeSet.getEntityDescriptor()));
            return changeSet;
        }
    }

    private static ObjectChangeSet copyChangeSet(ObjectChangeSet changeSet, Object original, Object clone,
                                                 Descriptor descriptor) {
        final ObjectChangeSet newChangeSet = ChangeSetFactory.createObjectChangeSet(original, clone, descriptor);
        changeSet.getChanges().forEach(newChangeSet::addChangeRecord);
        return newChangeSet;
    }

    private void evictAfterMerge(EntityType<?> et, URI identifier, Descriptor descriptor) {
        if (cacheManager.contains(et.getJavaType(), identifier, descriptor)) {
            cacheManager.evict(et.getJavaType(), identifier, descriptor.getSingleContext().orElse(null));
        }
        getMetamodel().getReferringTypes(et.getJavaType()).forEach(cacheManager::evict);
    }

    private void registerEntityWithPersistenceContext(Object entity) {
        if (isInCommit()) {
            return;
        }
        assert entity instanceof Manageable;
        ((Manageable) entity).setPersistenceContext(this);
    }

    private static void deregisterEntityFromPersistenceContext(Object entity) {
        if (!(entity instanceof Manageable)) {
            return;
        }
        ((Manageable) entity).setPersistenceContext(null);
    }

    @Override
    public NamedQueryManager getNamedQueryManager() {
        return parent.getNamedQueryManager();
    }

    @Override
    public ResultSetMappingManager getResultSetMappingManager() {
        return parent.getResultSetMappingManager();
    }

    @Override
    public Object registerExistingObject(Object entity, Descriptor descriptor) {
        return registerExistingObject(entity, descriptor, Collections.emptyList());
    }

    @Override
    public Object registerExistingObject(Object entity, Descriptor descriptor, List<Consumer<Object>> postClone) {
        if (entity == null) {
            return null;
        }
        if (cloneToOriginals.containsValue(entity)) {
            return getCloneForOriginal(entity);
        }
        final CloneConfiguration cloneConfig = new CloneConfiguration(descriptor, !isInCommit());
        postClone.forEach(cloneConfig::addPostRegisterHandler);
        Object clone = cloneBuilder.buildClone(entity, cloneConfig);
        assert clone != null;
        registerClone(clone, entity, descriptor);
        postClone.forEach(c -> c.accept(clone));
        return clone;
    }

    private void registerClone(Object clone, Object original, Descriptor descriptor) {
        cloneToOriginals.put(clone, original);
        final Object identifier = EntityPropertiesUtils.getIdentifier(clone, getMetamodel());
        keysToClones.put(identifier, clone);
        final InstanceDescriptor<?> instanceDesc = identifier != null ? InstanceDescriptorFactory.create(clone, (EntityType<Object>) entityType(clone.getClass())) : InstanceDescriptorFactory.createAllLoaded(clone, (EntityType<Object>) entityType(clone.getClass()));
        instanceDescriptors.put(clone, instanceDesc);
        registerEntityWithPersistenceContext(clone);
        registerEntityWithOntologyContext(clone, descriptor);
    }

    /**
     * Release this Unit of Work. Releasing an active Unit of Work with uncommitted changes causes all pending changes
     * to be discarded.
     */
    @Override
    public void release() {
        clear();
        storage.close();
        this.isActive = false;
        LOG.debug("UnitOfWork released.");
    }

    @Override
    public <T> void refreshObject(T object) {
        Objects.requireNonNull(object);
        ensureManaged(object);

        final IdentifiableEntityType<T> et = entityType((Class<T>) object.getClass());
        final URI idUri = EntityPropertiesUtils.getIdentifier(object, et);
        final Descriptor descriptor = getDescriptor(object);

        final LoadingParameters<T> params = new LoadingParameters<>(et.getJavaType(), idUri, descriptor, true);
        params.bypassCache();
        final ConnectionWrapper connection = acquireConnection();
        try {
            uowChangeSet.cancelObjectChanges(getOriginal(object));
            T original = connection.find(params);
            if (original == null) {
                throw new EntityNotFoundException("Entity " + object + " no longer exists in the repository.");
            }
            T source = (T) cloneBuilder.buildClone(original, new CloneConfiguration(descriptor, false));
            final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(source, object, descriptor);
            changeManager.calculateChanges(chSet);
            new RefreshInstanceMerger(indirectWrapperHelper).mergeChanges(chSet);
            revertTransactionalChanges(object, descriptor, chSet);
            registerClone(object, original, descriptor);
            et.getLifecycleListenerManager().invokePostLoadCallbacks(object);
        } finally {
            connection.close();
        }
    }

    private <T> void ensureManaged(T object) {
        if (!isObjectManaged(object)) {
            throw new IllegalArgumentException("Object not managed by this persistence context.");
        }
    }

    private <T> void revertTransactionalChanges(T object, Descriptor descriptor, ObjectChangeSet chSet) {
        for (ChangeRecord change : chSet.getChanges()) {
            storage.merge(object, (FieldSpecification<? super T, ?>) change.getAttribute(), descriptor.getAttributeDescriptor(change.getAttribute()));
        }
    }

    @Override
    public void registerNewObject(Object entity, Descriptor descriptor) {
        Objects.requireNonNull(entity);
        Objects.requireNonNull(descriptor);

        final IdentifiableEntityType<?> eType = entityType(entity.getClass());
        eType.getLifecycleListenerManager().invokePrePersistCallbacks(entity);
        Object id = initEntityIdentifier(entity, (EntityType<Object>) eType);
        assert id != null;
        verifyCanPersist(id, entity, eType, descriptor);
        // Original is null until commit
        newObjectsCloneToOriginal.put(entity, null);
        registerEntityWithOntologyContext(entity, descriptor);
        instanceDescriptors.put(entity, InstanceDescriptorFactory.createAllLoaded(entity, (EntityType<Object>) eType));
        newObjectsKeyToClone.put(id, entity);
        checkForIndirectObjects(entity);
        this.hasNew = true;
    }

    private Object initEntityIdentifier(Object entity, EntityType<Object> et) {
        Object id = getIdentifier(entity);
        if (id == null) {
            EntityPropertiesUtils.verifyIdentifierIsGenerated(entity, et);
            id = storage.generateIdentifier(et);
            EntityPropertiesUtils.setIdentifier(id, entity, et);
        }
        return id;
    }

    private void verifyCanPersist(Object id, Object instance, EntityType<?> et, Descriptor descriptor) {
        if (isIndividualManaged(id, instance) && !instance.getClass().isEnum()) {
            throw individualAlreadyManaged(id);
        }
        if (storage.contains(id, instance.getClass(), descriptor)) {
            throw new OWLEntityExistsException("Individual " + id + " of type " + et.getIRI() + " already exists in storage.");
        }
    }

    private boolean isIndividualManaged(Object identifier, Object entity) {
        return keysToClones.containsKey(identifier) || newObjectsKeyToClone.containsKey(identifier) && !cloneMapping.contains(entity);
    }

    @Override
    public void removeObject(Object entity) {
        assert entity != null;
        ensureManaged(entity);

        final IdentifiableEntityType<?> et = entityType(entity.getClass());
        et.getLifecycleListenerManager().invokePreRemoveCallbacks(entity);
        final Object primaryKey = getIdentifier(entity);
        final Descriptor descriptor = getDescriptor(entity);

        if (hasNew && newObjectsCloneToOriginal.containsKey(entity)) {
            unregisterObject(entity);
            newObjectsKeyToClone.remove(primaryKey);
        } else {
            deletedObjects.put(entity, entity);
            this.hasDeleted = true;
        }
        storage.remove(primaryKey, et.getJavaType(), descriptor);
        et.getLifecycleListenerManager().invokePostRemoveCallbacks(entity);
    }

    @Override
    public void restoreRemovedObject(Object entity) {
        assert deletedObjects.containsKey(entity);

        deletedObjects.remove(entity);
        final Object id = getIdentifier(entity);
        storage.persist(id, entity, getDescriptor(entity));
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
        final Object original = cloneToOriginals.remove(object);
        keysToClones.remove(EntityPropertiesUtils.getIdentifier(object, getMetamodel()));

        deletedObjects.remove(object);
        if (hasNew) {
            newObjectsCloneToOriginal.remove(object);
        }
        if (original != null) {
            cloneBuilder.removeVisited(original, repoMap.getEntityDescriptor(object));
        }
        removeIndirectWrappers(object);
        deregisterEntityFromPersistenceContext(object);
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
        // TODO This is a temporary workaround, configuration should be provided in constructor
        this.configuration = entityManager.getConfiguration();
    }

    @Override
    public void writeUncommittedChanges() {
        if (hasChanges()) {
            commitUnitOfWork();
        }
    }

    @Override
    public MetamodelImpl getMetamodel() {
        return parent.getMetamodel();
    }

    private <T> IdentifiableEntityType<T> entityType(Class<T> cls) {
        return getMetamodel().entity(cls);
    }

    @Override
    public boolean isEntityType(Class<?> cls) {
        return parent.isEntityType(cls);
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
    public <T> void loadEntityField(T entity, FieldSpecification<? super T, ?> fieldSpec) {
        Objects.requireNonNull(entity);
        Objects.requireNonNull(fieldSpec);
        final Field field = fieldSpec.getJavaField();
        assert field.getDeclaringClass().isAssignableFrom(entity.getClass());

        final Descriptor entityDescriptor = getDescriptor(entity);
        if (!instanceDescriptors.containsKey(entity)) {
            throw new OWLPersistenceException("Unable to find repository identifier for entity " + entity + ". Is it managed by this UoW?");
        }
        final InstanceDescriptor<?> instanceDescriptor = instanceDescriptors.get(entity);
        if (instanceDescriptor.isLoaded(fieldSpec) == LoadState.LOADED) {
            return;
        }

        storage.loadFieldValue(entity, fieldSpec, entityDescriptor);
        final Object orig = EntityPropertiesUtils.getFieldValue(field, entity);
        final Object entityOriginal = getOriginal(entity);
        if (entityOriginal != null) {
            EntityPropertiesUtils.setFieldValue(field, entityOriginal, orig);
        }
        final Descriptor fieldDescriptor = getFieldDescriptor(entity, field, entityDescriptor);
        final Object clone = cloneLoadedFieldValue(entity, field, fieldDescriptor, orig);
        EntityPropertiesUtils.setFieldValue(field, entity, clone);
        instanceDescriptors.get(entity).setLoaded(fieldSpec, LoadState.LOADED);
    }

    private <T> Descriptor getFieldDescriptor(T entity, Field field, Descriptor entityDescriptor) {
        final EntityType<?> et = entityType(entity.getClass());
        final FieldSpecification<?, ?> fieldSpec = et.getFieldSpecification(field.getName());
        return entityDescriptor.getAttributeDescriptor(fieldSpec);
    }

    private <T> Object cloneLoadedFieldValue(T entity, Field field, final Descriptor fieldDescriptor,
                                             final Object fieldValueOrig) {
        Object clone;
        if (fieldValueOrig == null) {
            clone = null;
        } else {
            if (isEntityType(field.getType())) {
                clone = registerExistingObject(fieldValueOrig, fieldDescriptor);
                putObjectIntoCache(getIdentifier(clone), fieldValueOrig, fieldDescriptor);
            } else {
                clone = cloneBuilder.buildClone(entity, field, fieldValueOrig, fieldDescriptor);
            }
        }
        return clone;
    }

    @Override
    public void removeObjectFromCache(Object toRemove, URI context) {
        Objects.requireNonNull(toRemove);

        cacheManager.evict(MetamodelUtils.getEntityClass(toRemove.getClass()), getIdentifier(toRemove), context);
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
    public <T> boolean isInferred(T entity, FieldSpecification<? super T, ?> attribute, Object value) {
        Objects.requireNonNull(entity);
        Objects.requireNonNull(attribute);
        Objects.requireNonNull(value);
        ensureManaged(entity);

        final Descriptor descriptor = getDescriptor(entity);
        return storage.isInferred(entity, attribute, value, descriptor);
    }

    @Override
    public LoadState isLoaded(Object entity, String attributeName) {
        Objects.requireNonNull(entity);
        final FieldSpecification<?, ?> fs = entityType(entity.getClass()).getFieldSpecification(attributeName);
        return instanceDescriptors.containsKey(entity) ? instanceDescriptors.get(entity)
                                                                            .isLoaded(fs) : LoadState.UNKNOWN;
    }

    @Override
    public LoadState isLoaded(Object entity) {
        Objects.requireNonNull(entity);
        return instanceDescriptors.containsKey(entity) ? instanceDescriptors.get(entity).isLoaded() : LoadState.UNKNOWN;
    }

    public SparqlQueryFactory sparqlQueryFactory() {
        return queryFactory;
    }

    public CriteriaBuilder criteriaFactory() {
        return criteriaFactory;
    }

    /**
     * Check if the specified entity contains a collection. If so, replace it with its indirect representation so that
     * changes in that collection can be tracked.
     *
     * @param entity The entity to check
     */
    private void checkForIndirectObjects(Object entity) {
        assert entity != null;
        final EntityType<?> et = entityType(entity.getClass());
        for (FieldSpecification<?, ?> fieldSpec : et.getFieldSpecifications()) {
            setIndirectObjectIfPresent(entity, fieldSpec.getJavaField());
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
    private void setIndirectObjectIfPresent(Object entity, Field field) {
        assert entity != null;
        assert field != null;

        final Object value = EntityPropertiesUtils.getFieldValue(field, entity);
        if (value instanceof IndirectWrapper) {
            return;
        }
        if (IndirectWrapperHelper.requiresIndirectWrapper(value)) {
            EntityPropertiesUtils.setFieldValue(field, entity, indirectWrapperHelper.createIndirectWrapper(value, entity, field));
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
    public Object createIndirectCollection(Object collection, Object owner, Field field) {
        return indirectWrapperHelper.createIndirectWrapper(collection, owner, field);
    }

    /**
     * Removes {@link IndirectWrapper} instances from the specified entity (if present).
     *
     * @param entity The entity to remove indirect wrappers from
     */
    private void removeIndirectWrappers(Object entity) {
        assert entity != null;
        final EntityType<?> et = entityType(entity.getClass());
        for (FieldSpecification<?, ?> fs : et.getFieldSpecifications()) {
            final Object value = EntityPropertiesUtils.getFieldValue(fs.getJavaField(), entity);
            if (value instanceof IndirectWrapper) {
                IndirectWrapper indirectWrapper = (IndirectWrapper) value;
                EntityPropertiesUtils.setFieldValue(fs.getJavaField(), entity, indirectWrapper.unwrap());
            }
        }
    }

    void putObjectIntoCache(Object identifier, Object entity, Descriptor descriptor) {
        cacheManager.add(identifier, entity, descriptor);
    }

    private Object getIdentifier(Object entity) {
        return EntityPropertiesUtils.getIdentifier(entity, getMetamodel());
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

    private void registerEntityWithOntologyContext(Object entity, Descriptor descriptor) {
        assert descriptor != null;
        assert entity != null;

        repoMap.add(descriptor, entity, null);
        repoMap.addEntityToRepository(entity, descriptor);
    }

    private boolean isInRepository(Descriptor descriptor, Object entity) {
        assert descriptor != null;
        assert entity != null;

        return repoMap.contains(descriptor, entity);
    }

    private Descriptor getDescriptor(Object entity) {
        assert entity != null;

        final Descriptor descriptor = repoMap.getEntityDescriptor(entity);
        if (descriptor == null) {
            throw new OWLPersistenceException("Unable to find descriptor of entity " + entity + " in this UoW!");
        }
        return descriptor;
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
    public <T> T unwrap(Class<T> cls) {
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        }
        return storage.unwrap(cls);
    }
}
