/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exceptions.EntityNotFoundException;
import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.proxy.IndirectWrapper;
import cz.cvut.kbss.jopa.sessions.cache.CacheManager;
import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.lifecycle.PostLoadInvoker;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryFactory;
import cz.cvut.kbss.jopa.sessions.cache.Descriptors;
import cz.cvut.kbss.jopa.sessions.change.Change;
import cz.cvut.kbss.jopa.sessions.change.ChangeCalculator;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.change.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadStateDescriptorRegistry;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.sessions.validator.InferredAttributeChangeValidator;
import cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.jopa.utils.MetamodelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;

import static cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException.individualAlreadyManaged;
import static cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator.getValidator;
import static cz.cvut.kbss.jopa.sessions.validator.IntegrityConstraintsValidator.isNotInferred;
import static cz.cvut.kbss.jopa.utils.EntityPropertiesUtils.getValueAsURI;

public abstract class AbstractUnitOfWork extends AbstractSession implements UnitOfWork {

    protected static final Logger LOG = LoggerFactory.getLogger(AbstractUnitOfWork.class);
    final IndirectWrapperHelper indirectWrapperHelper;

    // Read-only!!! It is just the keyset of cloneToOriginals
    final Set<Object> cloneMapping;
    final Map<Object, Object> cloneToOriginals;
    final Map<Object, Object> keysToClones = new HashMap<>();
    final Map<Object, Object> deletedObjects;
    final Map<Object, Object> newObjectsCloneToOriginal;
    final Map<Object, Object> newObjectsKeyToClone = new HashMap<>();
    private final Map<Object, Object> referenceProxies;
    RepositoryMap repoMap;

    final LoadStateDescriptorRegistry loadStateRegistry;

    boolean hasChanges;
    boolean hasNew;
    boolean hasDeleted;

    private boolean transactionActive;
    private boolean isActive;
    private boolean flushingChanges;

    UnitOfWorkChangeSet uowChangeSet = ChangeSetFactory.createUoWChangeSet();

    final AbstractSession parent;
    final ConnectionWrapper storage;

    final MergeManager mergeManager;
    final CloneBuilder cloneBuilder;
    final ChangeCalculator changeCalculator;
    final SparqlQueryFactory queryFactory;
    final InferredAttributeChangeValidator inferredAttributeChangeValidator;

    public AbstractUnitOfWork(AbstractSession parent, Configuration configuration) {
        super(configuration);
        this.parent = Objects.requireNonNull(parent);
        this.cloneToOriginals = new IdentityHashMap<>();
        this.cloneMapping = cloneToOriginals.keySet();
        this.deletedObjects = new IdentityHashMap<>();
        this.newObjectsCloneToOriginal = new IdentityHashMap<>();
        this.referenceProxies = new IdentityHashMap<>();
        this.repoMap = new RepositoryMap();
        this.loadStateRegistry = new LoadStateDescriptorRegistry(this::stringify);
        this.indirectWrapperHelper = new IndirectWrapperHelper(this);
        this.cloneBuilder = new CloneBuilder(this);
        this.storage = acquireConnection();
        this.queryFactory = new SparqlQueryFactory(this, storage);
        this.mergeManager = new MergeManager(this, cloneBuilder);
        this.changeCalculator = new ChangeCalculator(this);
        this.inferredAttributeChangeValidator = new InferredAttributeChangeValidator(storage);
        this.isActive = true;
    }

    @Override
    protected ConnectionWrapper acquireConnection() {
        final ConnectionWrapper conn = parent.acquireConnection();
        conn.setUnitOfWork(this);
        return conn;
    }

    @Override
    public void release() {
        clear();
        storage.close();
        this.isActive = false;
        LOG.debug("UnitOfWork released.");
    }

    @Override
    public void clear() {
        detachAllManagedInstances();
        cloneToOriginals.clear();
        keysToClones.clear();
        deletedObjects.clear();
        newObjectsCloneToOriginal.clear();
        newObjectsKeyToClone.clear();
        loadStateRegistry.clear();
        this.hasChanges = false;
        this.hasDeleted = false;
        this.hasNew = false;
        cloneBuilder.reset();
        this.repoMap = new RepositoryMap();
        repoMap.initDescriptors();
        this.uowChangeSet = ChangeSetFactory.createUoWChangeSet();
        this.transactionActive = false;
    }

    /**
     * Detaches all managed entities from this persistence context.
     */
    abstract void detachAllManagedInstances();

    @Override
    public CacheManager getLiveObjectCache() {
        return parent.getLiveObjectCache();
    }

    @Override
    public boolean isActive() {
        return isActive;
    }

    @Override
    public void begin() {
        this.transactionActive = true;
    }

    @Override
    public void commit() {
        LOG.trace("UnitOfWork commit started.");
        if (!isActive()) {
            throw new IllegalStateException("Cannot commit inactive Unit of Work!");
        }
        commitUnitOfWork();
        LOG.trace("UnitOfWork commit finished.");
    }

    /**
     * Commit this Unit of Work.
     */
    private void commitUnitOfWork() {
        this.flushingChanges = true;
        commitToStorage();
        mergeChangesIntoParent();
        postCommit();
    }

    void removeLazyLoadingProxies(Object entity) {
        assert entity != null;
        final EntityType<?> et = entityType(entity.getClass());
        for (FieldSpecification<?, ?> fs : et.getFieldSpecifications()) {
            final Object value = EntityPropertiesUtils.getFieldValue(fs.getJavaField(), entity);
            if (value instanceof LazyLoadingProxy<?> lazyLoadingProxy) {
                EntityPropertiesUtils.setFieldValue(fs.getJavaField(), entity, lazyLoadingProxy.unwrap());
            }
        }
    }

    /**
     * If there are any changes, commit them to the ontology.
     */
    abstract void commitToStorage();

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

    /**
     * Cleans up after the commit.
     */
    private void postCommit() {
        final boolean changes = hasChanges();
        clear();
        this.flushingChanges = false;
        if (changes) {
            getLiveObjectCache().evictInferredObjects();
        }
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

    @Override
    public boolean contains(Object entity) {
        Objects.requireNonNull(entity);
        return isObjectManaged(entity);
    }

    @Override
    public <T> T readObject(Class<T> cls, Object identifier, Descriptor descriptor) {
        Objects.requireNonNull(cls);
        Objects.requireNonNull(identifier);
        Objects.requireNonNull(descriptor);

        return readObjectInternal(cls, identifier, descriptor);
    }

    protected <T> T readObjectInternal(Class<T> cls, Object identifier, Descriptor descriptor) {
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
        final Object clone = registerExistingObject(result, new CloneRegistrationDescriptor(descriptor).postCloneHandlers(List.of(new PostLoadInvoker(getMetamodel()))));
        return cls.cast(clone);
    }

    <T> T readManagedObject(Class<T> cls, Object identifier, Descriptor descriptor) {
        // First try to find the object among new uncommitted objects
        Object result = newObjectsKeyToClone.get(identifier);
        if (result != null && (isInRepository(descriptor, result))) {
            // The result can be returned, since it is already registered in this UOW
            return cls.cast(result);
        }
        // Object is already managed
        return getManagedClone(cls, identifier, descriptor);
    }

    protected boolean isInRepository(Descriptor descriptor, Object entity) {
        assert descriptor != null;
        assert entity != null;

        return repoMap.contains(descriptor, entity);
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

    @Override
    public <T> T getReference(Class<T> cls, Object identifier, Descriptor descriptor) {
        Objects.requireNonNull(cls);
        Objects.requireNonNull(identifier);
        Objects.requireNonNull(descriptor);

        final T managed = readManagedObject(cls, identifier, descriptor);
        if (managed != null) {
            return managed;
        }
        if (keysToClones.containsKey(identifier)) {
            throw new EntityNotFoundException("Entity '" + cls.getSimpleName() + "' with id " + IdentifierTransformer.stringifyIri(identifier) + " not found.");
        }
        final T reference = storage.getReference(new LoadingParameters<>(cls, getValueAsURI(identifier), descriptor));
        registerEntityWithOntologyContext(reference, descriptor);
        referenceProxies.put(reference, reference);
        loadStateRegistry.put(reference, LoadStateDescriptorFactory.createNotLoaded(reference, entityType(cls)));
        return reference;
    }

    @Override
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
    public EntityState getState(Object entity) {
        Objects.requireNonNull(entity);

        if (deletedObjects.containsKey(entity)) {
            return EntityState.REMOVED;
        } else if (newObjectsCloneToOriginal.containsKey(entity)) {
            return EntityState.MANAGED_NEW;
        } else if (cloneMapping.contains(entity) || referenceProxies.containsKey(entity)) {
            return EntityState.MANAGED;
        } else {
            return EntityState.NOT_MANAGED;
        }
    }

    @Override
    public EntityState getState(Object entity, Descriptor descriptor) {
        Objects.requireNonNull(entity);
        Objects.requireNonNull(descriptor);

        if (deletedObjects.containsKey(entity)) {
            return EntityState.REMOVED;
        } else if (newObjectsCloneToOriginal.containsKey(entity) && isInRepository(descriptor, entity)) {
            return EntityState.MANAGED_NEW;
        } else if ((cloneMapping.contains(entity) || referenceProxies.containsKey(entity)) && isInRepository(descriptor, entity)) {
            return EntityState.MANAGED;
        } else {
            return EntityState.NOT_MANAGED;
        }
    }

    @Override
    public boolean isObjectNew(Object entity) {
        return entity != null && newObjectsCloneToOriginal.containsKey(entity);
    }

    @Override
    public boolean isObjectManaged(Object entity) {
        Objects.requireNonNull(entity);

        return (cloneMapping.contains(entity) || isManagedReference(entity)) && !deletedObjects.containsKey(entity)
                || newObjectsCloneToOriginal.containsKey(entity);
    }

    private boolean isManagedReference(Object entity) {
        return referenceProxies.containsKey(entity);
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

    Object getIdentifier(Object entity) {
        return EntityPropertiesUtils.getIdentifier(entity, getMetamodel());
    }

    private boolean isSameType(Object id, Object entity) {
        final Class<?> mergedType = entity.getClass();
        final Object managed = keysToClones.containsKey(id) ? keysToClones.get(id) : newObjectsKeyToClone.get(id);
        assert managed != null;
        final Class<?> managedType = MetamodelUtils.getEntityClass(managed.getClass());
        return managedType.isAssignableFrom(mergedType);
    }

    /**
     * Merges the specified detached entity into this persistence context.
     *
     * @param entity     Entity to merge
     * @param descriptor Descriptor of the merged entity
     * @param <T>        Entity type
     * @return Managed instance
     */
    abstract <T> T mergeDetachedInternal(T entity, Descriptor descriptor);

    @Override
    public Object registerExistingObject(Object entity, Descriptor descriptor) {
        return registerExistingObject(entity, new CloneRegistrationDescriptor(descriptor));
    }

    @Override
    public Object registerExistingObject(Object entity, CloneRegistrationDescriptor registrationDescriptor) {
        if (entity == null) {
            return null;
        }
        if (cloneToOriginals.containsValue(entity)) {
            return getCloneForOriginal(entity);
        }
        final CloneConfiguration cloneConfig = CloneConfiguration.withDescriptor(registrationDescriptor.getDescriptor())
                                                                 .forPersistenceContext(!isFlushingChanges())
                                                                 .addPostRegisterHandlers(registrationDescriptor.getPostCloneHandlers());
        Object clone = cloneBuilder.buildClone(entity, cloneConfig);
        assert clone != null;
        registerClone(clone, entity, registrationDescriptor.getDescriptor());
        registrationDescriptor.getPostCloneHandlers().forEach(c -> c.accept(clone));
        return clone;
    }

    void registerClone(Object clone, Object original, Descriptor descriptor) {
        cloneToOriginals.put(clone, original);
        final Object identifier = EntityPropertiesUtils.getIdentifier(clone, getMetamodel());
        keysToClones.put(identifier, clone);
        registerEntityWithOntologyContext(clone, descriptor);
    }

    protected <T> IdentifiableEntityType<T> entityType(Class<T> cls) {
        return getMetamodel().entity(cls);
    }

    /**
     * This method calculates the changes that were to the registered entities and adds these changes into the given
     * change set for future commit to the ontology.
     */
    void calculateChanges() {
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
            changeSet.addNewObjectChangeSet(ChangeSetFactory.createNewObjectChange(clone, c));
        }
    }

    private void calculateDeletedObjects(final UnitOfWorkChangeSet changeSet) {
        for (Object clone : deletedObjects.keySet()) {
            final Descriptor descriptor = getDescriptor(clone);
            Object original = getOriginal(clone);
            if (original == null) {
                assert referenceProxies.containsKey(clone);
                original = clone;
            }
            changeSet.addDeletedObjectChangeSet(ChangeSetFactory.createDeleteObjectChange(clone, original, descriptor));
            changeSet.cancelObjectChanges(original);
        }
    }

    void persistNewObjects() {
        if (hasNew) {
            newObjectsKeyToClone.forEach((id, entity) -> {
                final Descriptor descriptor = getDescriptor(entity);
                storage.persist(id, entity, descriptor);
                final IdentifiableEntityType<?> et = entityType(entity.getClass());
                et.getLifecycleListenerManager().invokePostPersistCallbacks(entity);
            });
        }
    }

    void validateIntegrityConstraints() {
        final IntegrityConstraintsValidator validator = getValidator();
        for (Change changeSet : uowChangeSet.getNewObjects()) {
            validator.validate(changeSet.getClone(), entityType((Class<Object>) changeSet.getObjectClass()), isNotInferred());
        }
        uowChangeSet.getExistingObjectsChanges().forEach(changeSet -> validator.validate(changeSet, getMetamodel()));
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
     * Registers the specified original for the specified clone, assuming the clone is a new object.
     * <p>
     * This method must be called during commit when new objects are persisted so that they can be referenced by other
     * objects.
     *
     * @param clone    Already registered clone
     * @param original Original to register
     */
    public void registerOriginalForNewClone(Object clone, Object original) {
        assert flushingChanges;
        assert newObjectsCloneToOriginal.containsKey(clone);
        newObjectsCloneToOriginal.put(clone, original);
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

    void preventCachingIfReferenceIsNotLoaded(ChangeRecord changeRecord) {
        final Object newValue = changeRecord.getNewValue();
        if (newValue != null && contains(newValue) && isLoaded(newValue) != LoadState.LOADED) {
            changeRecord.preventCaching();
        }
    }

    protected ObjectChangeSet processInferredValueChanges(ObjectChangeSet changeSet) {
        if (getConfiguration().is(JOPAPersistenceProperties.IGNORE_INFERRED_VALUE_REMOVAL_ON_MERGE)) {
            final ObjectChangeSet copy = ChangeSetFactory.createObjectChangeSet(changeSet.getOriginal(), changeSet.getClone(), changeSet.getDescriptor());
            changeSet.getChanges().stream().filter(chr -> !(chr.getAttribute().isInferred() &&
                    inferredAttributeChangeValidator.isInferredValueRemoval(changeSet.getClone(), changeSet.getOriginal(),
                            (FieldSpecification) chr.getAttribute(),
                            changeSet.getDescriptor()))).forEach(copy::addChangeRecord);
            return copy;
        } else {
            changeSet.getChanges().stream().filter(chr -> chr.getAttribute().isInferred()).forEach(
                    chr -> inferredAttributeChangeValidator.validateChange(changeSet.getClone(), changeSet.getOriginal(),
                            (FieldSpecification) chr.getAttribute(),
                            changeSet.getDescriptor()));
            return changeSet;
        }
    }

    protected <T> T getInstanceForMerge(URI identifier, EntityType<T> et, Descriptor descriptor) {
        if (keysToClones.containsKey(identifier)) {
            T managed = (T) keysToClones.get(identifier);
            et.getFieldSpecifications().stream().filter(fs -> fs.getFetchType() == FetchType.LAZY).forEach(fs -> {
                final Object fieldValue = EntityPropertiesUtils.getFieldValue(fs.getJavaField(), managed);
                if (fieldValue instanceof LazyLoadingProxy<?> proxy) {
                    proxy.triggerLazyLoading();
                }
            });
            return managed;
        }
        final LoadingParameters<T> params = new LoadingParameters<>(et.getJavaType(), identifier, descriptor, true);
        T original = storage.find(params);
        assert original != null;

        return (T) registerExistingObject(original, new CloneRegistrationDescriptor(descriptor).allEager(true));
    }

    protected void evictAfterMerge(EntityType<?> et, URI identifier, Descriptor descriptor) {
        if (getLiveObjectCache().contains(et.getJavaType(), identifier, descriptor)) {
            getLiveObjectCache().evict(et.getJavaType(), identifier, descriptor.getSingleContext().orElse(null));
        }
        getMetamodel().getReferringTypes(et.getJavaType()).forEach(getLiveObjectCache()::evict);
    }

    protected static ObjectChangeSet copyChangeSet(ObjectChangeSet changeSet, Object original, Object clone,
                                                   Descriptor descriptor) {
        final ObjectChangeSet newChangeSet = ChangeSetFactory.createObjectChangeSet(original, clone, descriptor);
        changeSet.getChanges().forEach(newChangeSet::addChangeRecord);
        return newChangeSet;
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
                throw new EntityNotFoundException("Entity " + stringify(object) + " no longer exists in the repository.");
            }
            removeLazyLoadingProxies(object);
            T source = (T) cloneBuilder.buildClone(original, CloneConfiguration.withDescriptor(descriptor));
            final ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(source, object, descriptor);
            changeCalculator.calculateChanges(chSet);
            new RefreshInstanceMerger(indirectWrapperHelper).mergeChanges(chSet);
            revertTransactionalChanges(object, descriptor, chSet);
            registerClone(object, original, descriptor);
            loadStateRegistry.put(object, LoadStateDescriptorFactory.createAllLoaded(object, et));
            et.getLifecycleListenerManager().invokePostLoadCallbacks(object);
        } finally {
            connection.close();
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
        loadStateRegistry.put(entity, LoadStateDescriptorFactory.createAllLoaded(entity, (EntityType<Object>) eType));
        newObjectsKeyToClone.put(id, entity);
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


    <T> void ensureManaged(T object) {
        if (!isObjectManaged(object)) {
            throw new IllegalArgumentException("Object not managed by this persistence context.");
        }
    }

    @Override
    public void restoreRemovedObject(Object entity) {
        assert deletedObjects.containsKey(entity);

        deletedObjects.remove(entity);
        final Object id = getIdentifier(entity);
        storage.persist(id, entity, getDescriptor(entity));
    }

    @Override
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
        unregisterEntityFromOntologyContext(object);
    }

    protected void unregisterEntityFromOntologyContext(Object entity) {
        assert entity != null;

        final Descriptor descriptor = repoMap.getEntityDescriptor(entity);
        if (descriptor == null) {
            throw new OWLPersistenceException("Fatal error, unable to find descriptor for entity " + stringify(entity));
        }
        repoMap.remove(descriptor, entity);
        repoMap.removeEntityToRepository(entity);
    }

    /**
     * Removes {@link IndirectWrapper} and {@link LazyLoadingProxy} instances from the specified entity (if present).
     *
     * @param entity The entity to remove indirect wrappers from
     */
    protected void removeIndirectWrappersAndProxies(Object entity) {
        assert entity != null;
        final EntityType<?> et = entityType(entity.getClass());
        for (FieldSpecification<?, ?> fs : et.getFieldSpecifications()) {
            final Object value = EntityPropertiesUtils.getFieldValue(fs.getJavaField(), entity);
            if (value instanceof IndirectWrapper indirectWrapper) {
                EntityPropertiesUtils.setFieldValue(fs.getJavaField(), entity, indirectWrapper.unwrap());
            } else if (value instanceof LazyLoadingProxy lazyLoadingProxy) {
                EntityPropertiesUtils.setFieldValue(fs.getJavaField(), entity, lazyLoadingProxy.unwrap());
            }
        }
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

    @Override
    public boolean isEntityType(Class<?> cls) {
        return getMetamodel().isEntityType(cls);
    }

    @Override
    public boolean isInTransaction() {
        return transactionActive;
    }

    @Override
    public boolean isFlushingChanges() {
        return flushingChanges;
    }

    @Override
    public <T> Object loadEntityField(T entity, FieldSpecification<? super T, ?> fieldSpec) {
        Objects.requireNonNull(entity);
        Objects.requireNonNull(fieldSpec);
        final Field field = fieldSpec.getJavaField();
        assert field.getDeclaringClass().isAssignableFrom(entity.getClass());

        final Descriptor entityDescriptor = getDescriptor(entity);
        final LoadStateDescriptor<?> loadStateDescriptor = loadStateRegistry.get(entity);
        if (loadStateDescriptor.isLoaded(fieldSpec) == LoadState.LOADED) {
            return EntityPropertiesUtils.getFieldValue(field, entity);
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
        loadStateDescriptor.setLoaded((FieldSpecification) fieldSpec, LoadState.LOADED);
        return clone;
    }

    /**
     * Gets basic object info for logging.
     * <p>
     * This works around using {@link Object#toString()} for entities, which could inadvertently trigger lazy field
     * fetching and cause an infinite field loading loop.
     *
     * @param object Object to stringify
     * @return String info about the specified object
     */
    public String stringify(Object object) {
        assert object != null;
        return isEntityType(object.getClass()) ?
                (object.getClass().getSimpleName() + IdentifierTransformer.stringifyIri(
                        EntityPropertiesUtils.getIdentifier(object, getMetamodel()))) :
                object.toString();
    }

    protected <T> Descriptor getFieldDescriptor(T entity, Field field, Descriptor entityDescriptor) {
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
                clone = registerExistingObject(fieldValueOrig, new CloneRegistrationDescriptor(fieldDescriptor));
                putObjectIntoCache(getIdentifier(clone), fieldValueOrig, fieldDescriptor);
            } else {
                clone = cloneBuilder.buildClone(entity, field, fieldValueOrig, fieldDescriptor);
            }
        }
        return clone;
    }

    @Override
    public void putObjectIntoCache(Object identifier, Object entity, Descriptor descriptor) {
        final LoadStateDescriptor<?> loadStateDescriptor = loadStateRegistry.get(entity);
        assert loadStateDescriptor != null;
        getLiveObjectCache().add(identifier, entity, new Descriptors(descriptor, loadStateDescriptor));
    }

    @Override
    public void removeObjectFromCache(Object toRemove, URI context) {
        Objects.requireNonNull(toRemove);

        getLiveObjectCache().evict(MetamodelUtils.getEntityClass(toRemove.getClass()), getIdentifier(toRemove), context);
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
        assert loadStateRegistry.contains(entity);
        if (loadStateRegistry.get(entity).isLoaded(attribute) == LoadState.NOT_LOADED) {
            value = loadEntityField(entity, attribute);
        }
        return storage.isInferred(entity, attribute, value, descriptor);
    }

    @Override
    public LoadState isLoaded(Object entity, String attributeName) {
        Objects.requireNonNull(entity);
        final FieldSpecification<?, ?> fs = entityType(entity.getClass()).getFieldSpecification(attributeName);
        return loadStateRegistry.contains(entity) ? loadStateRegistry.get(entity).isLoaded(fs) : LoadState.UNKNOWN;
    }

    @Override
    public LoadState isLoaded(Object entity) {
        Objects.requireNonNull(entity);
        return loadStateRegistry.contains(entity) ? loadStateRegistry.get(entity).isLoaded() : LoadState.UNKNOWN;
    }

    public SparqlQueryFactory sparqlQueryFactory() {
        return queryFactory;
    }

    public CriteriaBuilder getCriteriaBuilder() {
        return parent.getCriteriaBuilder();
    }

    void registerEntityWithOntologyContext(Object entity, Descriptor descriptor) {
        assert descriptor != null;
        assert entity != null;

        repoMap.add(descriptor, entity, null);
        repoMap.addEntityToRepository(entity, descriptor);
    }

    Descriptor getDescriptor(Object entity) {
        assert entity != null;

        final Descriptor descriptor = repoMap.getEntityDescriptor(entity);
        if (descriptor == null) {
            throw new OWLPersistenceException("Unable to find descriptor of entity " + stringify(entity) + " in this UoW!");
        }
        return descriptor;
    }

    public LoadStateDescriptorRegistry getLoadStateRegistry() {
        return loadStateRegistry;
    }

    @Override
    public <T> T unwrap(Class<T> cls) {
        if (cls.isAssignableFrom(getClass())) {
            return cls.cast(this);
        }
        return storage.unwrap(cls);
    }

    protected void markCloneForDeletion(Object entity, Object identifier) {
        if (hasNew && newObjectsCloneToOriginal.containsKey(entity)) {
            unregisterObject(entity);
            newObjectsKeyToClone.remove(identifier);
        } else {
            deletedObjects.put(entity, entity);
            this.hasDeleted = true;
        }
    }
}
