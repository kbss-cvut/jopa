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

import cz.cvut.kbss.jopa.model.EntityState;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.lifecycle.PostLoadInvoker;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxyFactory;
import cz.cvut.kbss.jopa.sessions.cache.DisabledCacheManager;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.sessions.cache.CacheManager;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException.individualAlreadyManaged;
import static cz.cvut.kbss.jopa.utils.EntityPropertiesUtils.getValueAsURI;

public class ReadOnlyUnitOfWork extends AbstractUnitOfWork {

    final Map<Object, Object> keysToOriginals = new HashMap<>();
    final Set<Object> originalMapping = new HashSet<>();

    private final LazyLoadingProxyFactory lazyLoaderFactory;
    private CacheManager liveObjectCache;

    ReadOnlyUnitOfWork(AbstractSession parent, Configuration configuration) {
        super(parent, configuration);
        this.lazyLoaderFactory = new LazyLoadingProxyFactory(this);
        this.liveObjectCache = resolveCacheManager();
    }

    @Override
    public void clear() {
        LOG.trace("Clearing read-only UOW.");

        super.clear();
        keysToOriginals.clear();
        originalMapping.clear();
    }

    @Override
    void detachAllManagedInstances() {
        originalMapping.forEach(super::removeLazyLoadingProxies);
    }

    @Override
    public void commit() {
        LOG.trace("Read-only UnitOfWork commit started.");
        if (!isActive()) {
            throw new IllegalStateException("Cannot commit inactive Unit of Work!");
        }
        this.clear();
        LOG.trace("UnitOfWork commit finished.");
    }

    @Override
    protected <T> T readObjectInternal(Class<T> cls, Object identifier, Descriptor descriptor) {
        assert cls != null;
        assert identifier != null;
        assert descriptor != null;

        // check managed objects
        T result = readManagedObject(cls, identifier, descriptor);
        if (result != null) {
            return result;
        }
        LoadingParameters<T> params = new LoadingParameters<>(cls, getValueAsURI(identifier), descriptor);

        // registered result is either original or clone of original (if original is read from cache)
        Object registeredResult;
        if (getLiveObjectCache().contains(cls, identifier, descriptor)) {
            result = storage.find(params);
            LOG.trace("Read cached object {} with identifier {} is cached. Cloning it.", result, identifier);
            registeredResult = registerExistingObject(result, new CloneRegistrationDescriptor(descriptor)
                    .postCloneHandlers(List.of(new PostLoadInvoker(getMetamodel())))
            );
        } else {
            params.bypassCache();
            result = storage.find(params);
            LOG.trace("Read object {} with identifier {}.", result, identifier);
            registeredResult = registerExistingObject(result, descriptor);
        }
        return cls.cast(registeredResult);
    }

    @Override
    <T> T readManagedObject(Class<T> cls, Object identifier, Descriptor descriptor) {
        // simply return the managed object or null
        return getManagedOriginal(cls, identifier, descriptor);
    }

    @Override
    public <T> T getManagedOriginal(Class<T> cls, Object identifier, Descriptor descriptor) {
        if (!this.keysToOriginals.containsKey(identifier)) { return null; }

        final Object original = keysToOriginals.get(identifier);
        if (!cls.isAssignableFrom(original.getClass())) {
            throw individualAlreadyManaged(identifier);
        }

        return isInRepository(descriptor, original) ? cls.cast(original) : null;
    }

    @Override
    public void unregisterObject(Object object) {
        if (object == null) { return; }

        originalMapping.remove(object);
        keysToOriginals.remove(super.getIdentifier(object));

        super.removeLazyLoadingProxies(object);
        super.unregisterEntityFromOntologyContext(object);
    }

    @Override
    public Object registerExistingObject(Object entity, Descriptor descriptor) {
        LOG.trace("Registering object {}", entity);
        if (entity == null) { return null; }

        if (containsOriginal(entity)) {
            LOG.trace("Original object {} already registered", entity);
            return entity;
        }

        registerEntity(entity, descriptor);
        processEntityFields(entity);
        List.of(new PostLoadInvoker(getMetamodel())).forEach(c -> c.accept(entity));
        return entity;
    }

    @Override
    public Object registerExistingObject(Object entity, CloneRegistrationDescriptor registrationDescriptor) {
        if (entity == null) { return null; }

        final CloneConfiguration cloneConfig = CloneConfiguration.withDescriptor(registrationDescriptor.getDescriptor())
                                                                 .addPostRegisterHandlers(registrationDescriptor.getPostCloneHandlers());
        Object clone = cloneBuilder.buildClone(entity, cloneConfig);
        assert clone != null;

        registerEntity(clone, registrationDescriptor.getDescriptor());
        registrationDescriptor.getPostCloneHandlers().forEach(c -> c.accept(clone));
        return clone;
    }

    private void registerEntity(Object entity, Descriptor descriptor) {
        originalMapping.add(entity);
        final Object identifier = super.getIdentifier(entity);
        keysToOriginals.put(identifier, entity);

        super.registerEntityWithOntologyContext(entity, descriptor);

        if (super.isEntityType(entity.getClass()) && !super.getLoadStateRegistry().contains(entity)) {
            super.getLoadStateRegistry().put(
                entity,
                LoadStateDescriptorFactory.createAllUnknown(entity, (EntityType<? super Object>) getMetamodel().entity(entity.getClass())));
        }
    }

    private void processEntityFields(Object original) {
        // inject lazy loading proxies and process relationships
        final Class<?> originalClass = original.getClass();
        final EntityType<?> et = getMetamodel().entity(originalClass);
        final LoadStateDescriptor<Object> loadState = super.getLoadStateRegistry().get(original);

        for (FieldSpecification<?, ?> fs : et.getFieldSpecifications()) {
            if (fs == et.getIdentifier()) { continue; }   // Already cloned

            final Field f = fs.getJavaField();
            final Object fieldValue = EntityPropertiesUtils.getFieldValue(f, original);
            Object newValue = null;

            if (loadState.isLoaded(fs) == LoadState.NOT_LOADED) {
                newValue = lazyLoaderFactory.createProxy(original, (FieldSpecification<? super Object, ?>) fs);
            } else if (fieldValue == null) {
                continue;
            } else {
                final Class<?> fieldValueClass = fieldValue.getClass();

                if (IndirectWrapperHelper.requiresIndirectWrapper(fieldValue)) {
                    // register objects if possible
                    Descriptor fieldDescriptor = super.getDescriptor(original).getAttributeDescriptor(fs);
                    if (fs.isCollection()) {
                        this.registerExistingObjects((Iterable<Object>) fieldValue, fieldDescriptor);
                    }
                    newValue = fieldValue;
                } else if (super.isEntityType(fieldValueClass)) {
                    final Descriptor entityDescriptor = super.getDescriptor(original);
                    final Descriptor fieldDescriptor = super.getFieldDescriptor(original, f, entityDescriptor);
                    if (this.getLiveObjectCache().contains(fieldValueClass, super.getIdentifier(fieldValue), fieldDescriptor)) {
                        newValue = registerExistingObject(fieldValue, new CloneRegistrationDescriptor(fieldDescriptor));
                    } else{
                        newValue = registerExistingObject(fieldValue, fieldDescriptor);
                    }
                } else {
                    // We assume that the value is immutable
                    newValue = fieldValue;
                }
            }
            EntityPropertiesUtils.setFieldValue(f, original, newValue);
        }
    }

    private void registerExistingObjects(Iterable<Object> collection, Descriptor descriptor) {
        collection.forEach(obj -> {
            if (!super.isEntityType(obj.getClass())) { return; }
            registerExistingObject(obj, descriptor);
        });
    }

    @Override
    public Object getOriginal(Object original) {
        // simply return the original object
        // TODO: change javadoc
        return original;
    }

    @Override
    boolean containsOriginal(Object entity) {
        assert entity != null;
        return originalMapping.contains(entity);
    }

    @Override
    public boolean isObjectManaged(Object entity) {
        Objects.requireNonNull(entity);
        return this.originalMapping.contains(entity);
    }

    @Override
    public EntityState getState(Object entity) {
        Objects.requireNonNull(entity);
        return originalMapping.contains(entity) ? EntityState.MANAGED : EntityState.NOT_MANAGED;
    }

    @Override
    public EntityState getState(Object entity, Descriptor descriptor) {
        Objects.requireNonNull(entity);
        Objects.requireNonNull(descriptor);

         return originalMapping.contains(entity) && super.isInRepository(descriptor, entity)
                 ? EntityState.MANAGED
                 : EntityState.NOT_MANAGED;
    }

    @Override
    public <T> T getReference(Class<T> cls, Object identifier, Descriptor descriptor) {
        return super.readObject(cls, identifier, descriptor);
    }

    @Override
    public Object getCloneForOriginal(Object original) {
        // this unit of work does not track clone-original
        // simply return the original object
        return original;
    }

    @Override
    public Object createIndirectCollection(Object collection, Object owner, Field field) {
        // Do not create any special kind of collection, just return the argument
        return collection;
    }

    private CacheManager resolveCacheManager() {
        final String enabledStr = this.configuration.get(JOPAPersistenceProperties.CACHE_ENABLED_READ_ONLY);
        return (enabledStr != null && !Boolean.parseBoolean(enabledStr))
                ? new DisabledCacheManager()
                : this.parent.getLiveObjectCache();
    }

    @Override
    public CacheManager getLiveObjectCache() {
        return super.getLiveObjectCache();
    }

    @Override
    public void putObjectIntoCache(Object identifier, Object entity, Descriptor descriptor) {
        // object should never be put into cached in this uow
    }

    //////////////////////////////////////THESE METHODS SHOULD NOT BE SUPPORTED/////////////////////////////////////////
    @Override
    public void removeObjectFromCache(Object toRemove, URI context) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    void preventCachingIfReferenceIsNotLoaded(ChangeRecord changeRecord) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public boolean isObjectNew(Object entity) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public <T> T mergeDetached(T entity, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public <T> T mergeDetachedInternal(T entity, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    protected <T> T getInstanceForMerge(URI identifier, EntityType<T> et, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    protected void evictAfterMerge(EntityType<?> et, URI identifier, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public <T> void refreshObject(T object) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    protected static ObjectChangeSet copyChangeSet(ObjectChangeSet changeSet, Object original, Object clone,
                                                   Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    protected ObjectChangeSet processInferredValueChanges(ObjectChangeSet changeSet) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    void validateIntegrityConstraints() {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    void calculateChanges() {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public void commitToStorage() {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    void persistNewObjects() {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    void registerClone(Object clone, Object original, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public void registerOriginalForNewClone(Object clone, Object original) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public void writeUncommittedChanges() {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public boolean hasChanges() {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    void setHasChanges() {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public void restoreRemovedObject(Object entity) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public boolean isFlushingChanges() {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public void attributeChanged(Object entity, Field f) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public void attributeChanged(Object entity, FieldSpecification<?, ?> fieldSpec) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    protected void markCloneForDeletion(Object entity, Object identifier) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public void registerNewObject(Object entity, Descriptor descriptor) {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public void removeObject(Object object) {
        throw new UnsupportedOperationException("Method not supported.");
    }
}
