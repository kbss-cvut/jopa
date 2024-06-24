/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exceptions.OWLEntityExistsException;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.Manageable;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.proxy.IndirectWrapper;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.validator.AttributeModificationValidator;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;
import java.net.URI;

public class ChangeTrackingUnitOfWork extends AbstractUnitOfWork {

    public ChangeTrackingUnitOfWork(AbstractSession parent, Configuration configuration) {
        super(parent, configuration);
    }

    @Override
    protected <T> T readObjectInternal(Class<T> cls, Object identifier, Descriptor descriptor) {
        final T clone = super.readObjectInternal(cls, identifier, descriptor);
        if (clone != null) {
            checkForIndirectObjects(clone);
        }
        return clone;
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
     * If the specified field is of Collection type, and it is not already an indirect collection, create new one and
     * set it as the value of the specified field on the specified entity.
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
     * If there are any changes, commit them to the ontology.
     */
    void commitToStorage() {
        if (this.hasNew || this.hasChanges || this.hasDeleted) {
            persistNewObjects();
            calculateChanges();
        }
        validateIntegrityConstraints();
        storage.commit();
    }

    @Override
    protected void detachAllManagedInstances() {
        cloneMapping.forEach(instance -> {
            removeIndirectWrappersAndProxies(instance);
            deregisterEntityFromPersistenceContext(instance);
        });
        newObjectsCloneToOriginal.keySet().forEach(this::removeIndirectWrappersAndProxies);
    }

    /**
     * Removes {@link IndirectWrapper} and {@link LazyLoadingProxy} instances from the specified entity (if present).
     *
     * @param entity The entity to remove indirect wrappers from
     */
    private void removeIndirectWrappersAndProxies(Object entity) {
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
    void registerClone(Object clone, Object original, Descriptor descriptor) {
        super.registerClone(clone, original, descriptor);
        attachPersistenceContextToEntity(clone);
    }

    private void attachPersistenceContextToEntity(Object entity) {
        if (isFlushingChanges()) {
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
    public void attributeChanged(Object entity, Field f) {
        final IdentifiableEntityType<Object> et = entityType((Class<Object>) entity.getClass());
        final FieldSpecification<Object, ?> fieldSpec = et.getFieldSpecification(f.getName());
        attributeChanged(entity, fieldSpec);
    }

    @Override
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
        ((LoadStateDescriptor) loadStateRegistry.get(entity)).setLoaded(fieldSpec, LoadState.LOADED);
    }

    private void createAndRegisterChangeRecord(Object clone, FieldSpecification<?, ?> fieldSpec,
                                               Descriptor descriptor) {
        final Object orig = getOriginal(clone);
        if (orig == null) {
            return;
        }
        final ChangeRecord record = new ChangeRecord(fieldSpec, EntityPropertiesUtils.getFieldValue(fieldSpec.getJavaField(), clone));
        preventCachingIfReferenceIsNotLoaded(record);
        registerChangeRecord(clone, orig, descriptor, record);
    }

    private void registerChangeRecord(Object clone, Object orig, Descriptor descriptor, ChangeRecord record) {
        ObjectChangeSet chSet = uowChangeSet.getExistingObjectChanges(orig);
        if (chSet == null) {
            chSet = ChangeSetFactory.createObjectChangeSet(orig, clone, descriptor);
            uowChangeSet.addObjectChangeSet(chSet);
        }
        chSet.addChangeRecord(record);
    }

    <T> T mergeDetachedInternal(T entity, Descriptor descriptor) {
        assert entity != null;
        final IdentifiableEntityType<T> et = (IdentifiableEntityType<T>) entityType(entity.getClass());
        final URI idUri = EntityPropertiesUtils.getIdentifier(entity, et);

        final T clone = getInstanceForMerge(idUri, et, descriptor);
        try {
            ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(clone, entity, descriptor);
            // Merge only the changed attributes
            changeCalculator.calculateChanges(chSet);
            // Have to check for inferred attribute changes before the actual merge
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
                registerMergeChangeSet(chSet, clone, descriptor);
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

    private <T> void registerMergeChangeSet(ObjectChangeSet mergeChangeSet, T clone, Descriptor descriptor) {
        final Object original = getOriginal(clone);
        if (uowChangeSet.getExistingObjectChanges(original) != null) {
            final ObjectChangeSet existingChSet = uowChangeSet.getExistingObjectChanges(original);
            mergeChangeSet.getChanges().forEach(existingChSet::addChangeRecord);
        } else {
            uowChangeSet.addObjectChangeSet(copyChangeSet(mergeChangeSet, original, clone, descriptor));
        }
    }

    @Override
    public void registerNewObject(Object entity, Descriptor descriptor) {
        super.registerNewObject(entity, descriptor);
        checkForIndirectObjects(entity);
    }

    @Override
    public void unregisterObject(Object object) {
        super.unregisterObject(object);
        removeIndirectWrappersAndProxies(object);
        deregisterEntityFromPersistenceContext(object);
    }

    @Override
    public void removeObject(Object entity) {
        assert entity != null;
        ensureManaged(entity);

        final IdentifiableEntityType<?> et = entityType(entity.getClass());
        et.getLifecycleListenerManager().invokePreRemoveCallbacks(entity);
        final Object identifier = getIdentifier(entity);
        // Get the descriptor before clone is removed
        final Descriptor descriptor = getDescriptor(entity);

        markCloneForDeletion(entity, identifier);
        storage.remove(identifier, et.getJavaType(), descriptor);
        et.getLifecycleListenerManager().invokePostRemoveCallbacks(entity);
    }
}
