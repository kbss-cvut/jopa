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
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.lifecycle.PostLoadInvoker;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxyFactory;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptorFactory;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import cz.cvut.kbss.jopa.sessions.util.CloneRegistrationDescriptor;
import cz.cvut.kbss.jopa.sessions.util.LoadingParameters;
import cz.cvut.kbss.jopa.utils.Configuration;
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

    ReadOnlyUnitOfWork(AbstractSession parent, Configuration configuration) {
        super(parent, configuration);
        this.lazyLoaderFactory = new LazyLoadingProxyFactory(this);
    }

    @Override
    public void clear() {
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
        LOG.trace("Read-only UnitOfWork commit started. Nothing is commited to a database.");
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
        if (isObjectInCache(cls, identifier, descriptor)) {
            result = storage.find(params);
            registeredResult = registerExistingObject(result, new CloneRegistrationDescriptor(descriptor)
                    .postCloneHandlers(List.of(new PostLoadInvoker(getMetamodel())))
            );
        } else {
            params.bypassCache();
            result = storage.find(params);
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
        if (entity == null) { return null; }

        if (containsOriginal(entity)) {
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

                    if (isObjectManaged(fieldValue)) {
                        newValue = fieldValue;
                    } else if (isObjectInCache(fieldValueClass, super.getIdentifier(fieldValue), fieldDescriptor)) {
                        newValue = registerExistingObject(fieldValue, new CloneRegistrationDescriptor(fieldDescriptor)
                                .postCloneHandlers(List.of(new PostLoadInvoker(getMetamodel()))));
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
        for (Object entity : collection) {
            if (!super.isEntityType(entity.getClass()) || isObjectManaged(entity)) { return; }
            if (isObjectInCache(entity.getClass(), super.getIdentifier(entity), descriptor)) {
                registerExistingObject(entity, new CloneRegistrationDescriptor(descriptor)
                        .postCloneHandlers(List.of(new PostLoadInvoker(getMetamodel()))));
            } else {
                registerExistingObject(entity, descriptor);
            }
        }
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

    private boolean isObjectInCache(Class<?> cls, Object identifier, Descriptor descriptor) {
        return getLiveObjectCache().contains(cls, identifier, descriptor);
    }

    @Override
    public void putObjectIntoCache(Object identifier, Object entity, Descriptor descriptor) {
        // object should never be put into cached in this uow
    }

    //////////////////////////////////////THESE METHODS SHOULD NOT BE SUPPORTED/////////////////////////////////////////
    private static void throwUnsupportedOperationException() {
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    public void removeObjectFromCache(Object toRemove, URI context) {
        throwUnsupportedOperationException();
    }

    @Override
    void preventCachingIfReferenceIsNotLoaded(ChangeRecord changeRecord) {
        throwUnsupportedOperationException();
    }

    @Override
    public boolean isObjectNew(Object entity) {
        throwUnsupportedOperationException();
        return false;
    }

    @Override
    public <T> T mergeDetached(T entity, Descriptor descriptor) {
        throwUnsupportedOperationException();
        return null;
    }

    @Override
    public <T> T mergeDetachedInternal(T entity, Descriptor descriptor) {
        throwUnsupportedOperationException();
        return null;
    }

    @Override
    protected <T> T getInstanceForMerge(URI identifier, EntityType<T> et, Descriptor descriptor) {
        throwUnsupportedOperationException();
        return null;
    }

    @Override
    protected void evictAfterMerge(EntityType<?> et, URI identifier, Descriptor descriptor) {
        throwUnsupportedOperationException();
    }

    @Override
    public <T> void refreshObject(T object) {
        throwUnsupportedOperationException();
    }

    protected static ObjectChangeSet copyChangeSet(ObjectChangeSet changeSet, Object original, Object clone,
                                                   Descriptor descriptor) {
        throwUnsupportedOperationException();
        return null;
    }

    @Override
    protected ObjectChangeSet processInferredValueChanges(ObjectChangeSet changeSet) {
        throwUnsupportedOperationException();
        return null;
    }

    @Override
    void validateIntegrityConstraints() {
        throwUnsupportedOperationException();
    }

    @Override
    void calculateChanges() {
        throwUnsupportedOperationException();
    }

    @Override
    public void commitToStorage() {
        // TODO
//        storage.commit();
        throw new UnsupportedOperationException("Method not supported.");
    }

    @Override
    void persistNewObjects() {
        throwUnsupportedOperationException();
    }

    @Override
    void registerClone(Object clone, Object original, Descriptor descriptor) {
        throwUnsupportedOperationException();
    }

    @Override
    public void registerOriginalForNewClone(Object clone, Object original) {
        throwUnsupportedOperationException();
    }

    @Override
    public void writeUncommittedChanges() {
        throwUnsupportedOperationException();
    }

    @Override
    public boolean hasChanges() {
        throwUnsupportedOperationException();
        return false;
    }

    @Override
    void setHasChanges() {
        throwUnsupportedOperationException();
    }

    @Override
    public void restoreRemovedObject(Object entity) {
        throwUnsupportedOperationException();
    }

    @Override
    public boolean isFlushingChanges() {
        throwUnsupportedOperationException();
        return false;
    }

    @Override
    public void attributeChanged(Object entity, Field f) {
        throwUnsupportedOperationException();
    }

    @Override
    public void attributeChanged(Object entity, FieldSpecification<?, ?> fieldSpec) {
        throwUnsupportedOperationException();
    }

    @Override
    protected void markCloneForDeletion(Object entity, Object identifier) {
        throwUnsupportedOperationException();
    }

    @Override
    public void registerNewObject(Object entity, Descriptor descriptor) {
        throwUnsupportedOperationException();
    }

    @Override
    public void removeObject(Object object) {
        throwUnsupportedOperationException();
    }
}
