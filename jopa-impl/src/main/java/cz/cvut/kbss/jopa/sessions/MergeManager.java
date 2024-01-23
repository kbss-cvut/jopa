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

import cz.cvut.kbss.jopa.api.ChangeRecord;
import cz.cvut.kbss.jopa.api.ObjectChangeSet;
import cz.cvut.kbss.jopa.api.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.util.Objects;

/**
 * Merges changes that are made to clones to the registered original objects and live object cache.
 */
public class MergeManager {

    private final UnitOfWorkImpl uow;

    private final CloneBuilder builder;

    MergeManager(UnitOfWorkImpl session) {
        this.uow = session;
        this.builder = session.getCloneBuilder();
    }

    private void deleteObjectFromCache(ObjectChangeSet changeSet) {
        Object toDelete = changeSet.getChangedObject();
        assert toDelete != null;
        uow.removeObjectFromCache(toDelete, changeSet.getEntityContext());
    }

    /**
     * Merge changes from one {@link ObjectChangeSet}, which represents the changes made to clone, into the original
     * object.
     *
     * @param changeSet ObjectChangeSet containing changes on a single object
     */
    public void mergeChangesOnObject(ObjectChangeSet changeSet) {
        Objects.requireNonNull(changeSet);
        final Object clone = changeSet.getCloneObject();
        if (clone == null) {
            return;
        }
        if (changeSet.getChangedObject() == null) {
            // If the original is null, then we may have a new object
            // but this should not happen since new objects are handled separately
            if (uow.isObjectNew(clone)) {
                mergeNewObject(changeSet);
            } else {
                throw new OWLPersistenceException("Cannot find the original object.");
            }
        } else {
            builder.mergeChanges(changeSet);
            updateCache(changeSet);
        }
    }

    private void updateCache(ObjectChangeSet changeSet) {
        final Object changedObject = changeSet.getChangedObject();
        final Object identifier = EntityPropertiesUtils.getIdentifier(changedObject, uow.getMetamodel());
        if (changeSet.isNew()) {
            uow.putObjectIntoCache(identifier, changedObject, changeSet.getEntityDescriptor());
        } else {
            boolean preventCaching = changeSet.getChanges().stream().anyMatch(ChangeRecord::doesPreventCaching);
            if (preventCaching) {
                uow.removeObjectFromCache(changedObject, changeSet.getEntityContext());
            } else {
                uow.putObjectIntoCache(identifier, changedObject, changeSet.getEntityDescriptor());
            }
        }
    }

    /**
     * Merge changes from the provided {@link UnitOfWorkChangeSet}.
     *
     * @param changeSet Change set from a single Unit of Work
     */
    public void mergeChangesFromChangeSet(UnitOfWorkChangeSet changeSet) {
        Objects.requireNonNull(changeSet);
        for (ObjectChangeSet objectChangeSet : changeSet.getExistingObjectsChanges()) {
            mergeChangesOnObject(objectChangeSet);
        }
        changeSet.getNewObjects().forEach(this::mergeNewObject);
        changeSet.getDeletedObjects().forEach(this::deleteObjectFromCache);

    }

    /**
     * Merge a newly created object represented by an {@link ObjectChangeSet} into the shared live object cache.
     *
     * @param changeSet ObjectChangeSet representing the new object
     */
    public void mergeNewObject(ObjectChangeSet changeSet) {
        Objects.requireNonNull(changeSet);
        assert changeSet.isNew();
        // Put the original object into the live object cache
        updateCache(changeSet);
    }
}
