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
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.lifecycle.LifecycleEvent;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.sessions.change.ChangeSetFactory;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.validator.AttributeModificationValidator;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;
import java.net.URI;

public class OnCommitChangePropagatingUnitOfWork extends AbstractUnitOfWork {

    OnCommitChangePropagatingUnitOfWork(AbstractSession parent, Configuration configuration) {
        super(parent, configuration);
    }

    @Override
    void detachAllManagedInstances() {
        cloneMapping.forEach(this::removeLazyLoadingProxies);
    }

    @Override
    void commitToStorage() {
        calculateChanges();
        if (this.hasNew) {
            persistNewObjects();
        }
        uowChangeSet.getExistingObjectsChanges().forEach(chSet -> {
            final IdentifiableEntityType<?> et = entityType(chSet.getObjectClass());
            final Object entity = chSet.getClone();
            et.getLifecycleListenerManager().invokePreUpdateCallbacks(entity);
            if (et.getLifecycleListenerManager().hasLifecycleCallback(LifecycleEvent.PRE_UPDATE)) {
                // Recalculate changes if a preUpdate callback was called as it may have altered the entity state
                changeCalculator.calculateChanges(chSet);
            }
            chSet.getChanges()
                 .forEach(record -> {
                     AttributeModificationValidator.verifyCanModify(record.getAttribute());
                     preventCachingIfReferenceIsNotLoaded(record);
                     storage.merge(entity, (FieldSpecification<? super Object, ?>) record.getAttribute(), chSet.getDescriptor());
                 });
            et.getLifecycleListenerManager().invokePostUpdateCallbacks(entity);
        });
        uowChangeSet.getDeletedObjects().forEach(chSet -> {
            final IdentifiableEntityType<?> et = entityType(chSet.getObjectClass());
            final Object identifier = getIdentifier(chSet.getClone());
            storage.remove(identifier, chSet.getObjectClass(), chSet.getDescriptor());
            et.getLifecycleListenerManager().invokePostRemoveCallbacks(chSet.getClone());
        });
        validateIntegrityConstraints();
        storage.commit();
    }

    @Override
    void calculateChanges() {
        super.calculateChanges();
        cloneToOriginals.entrySet().stream().filter(e -> !deletedObjects.containsKey(e.getKey())).forEach(e -> {
            final Object original = e.getValue();
            final Object clone = e.getKey();
            ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(original, clone, getDescriptor(clone));
            changeCalculator.calculateChanges(chSet);
            processInferredValueChanges(chSet);
            if (chSet.hasChanges()) {
                uowChangeSet.addObjectChangeSet(chSet);
            }
        });
        if (uowChangeSet.hasChanges()) {
            setHasChanges();
        }
    }

    @Override
    <T> T mergeDetachedInternal(T toMerge, Descriptor descriptor) {
        assert toMerge != null;
        final IdentifiableEntityType<T> et = (IdentifiableEntityType<T>) entityType(toMerge.getClass());
        final URI idUri = EntityPropertiesUtils.getIdentifier(toMerge, et);

        final T clone = getInstanceForMerge(idUri, et, descriptor);
        try {
            ObjectChangeSet chSet = ChangeSetFactory.createObjectChangeSet(clone, toMerge, descriptor);
            changeCalculator.calculateChanges(chSet);
            chSet = processInferredValueChanges(chSet);
            if (chSet.hasChanges()) {
                final DetachedInstanceMerger merger = new DetachedInstanceMerger(this);
                merger.mergeChangesFromDetachedToManagedInstance(chSet, descriptor);
                uowChangeSet.addObjectChangeSet(copyChangeSet(chSet, getOriginal(clone), clone, descriptor));
            }
        } catch (OWLEntityExistsException e) {
            unregisterObject(clone);
            throw e;
        }
        evictAfterMerge(et, idUri, descriptor);
        setHasChanges();
        return et.getJavaType().cast(clone);
    }

    @Override
    public void removeObject(Object entity) {
        assert entity != null;
        ensureManaged(entity);

        final Object identifier = getIdentifier(entity);
        markCloneForDeletion(entity, identifier);
    }

    @Override
    public void unregisterObject(Object object) {
        super.unregisterObject(object);
        removeLazyLoadingProxies(object);
    }

    @Override
    public void attributeChanged(Object entity, Field f) {
        // Do nothing
    }

    @Override
    public void attributeChanged(Object entity, FieldSpecification<?, ?> fieldSpec) {
        // Do nothing
    }

    @Override
    public Object createIndirectCollection(Object collection, Object owner, Field field) {
        // Do not create any special kind of collection, just return the argument
        return collection;
    }

    @Override
    public void setReadOnly(boolean readOnly) throws Exception {
        throw new Exception("Read only mode for this unit of work is not supported.");
    }

    @Override
    public boolean isReadOnly() throws Exception {
        throw new Exception("Read only mode for this unit of work is not supported.");
    }
}
