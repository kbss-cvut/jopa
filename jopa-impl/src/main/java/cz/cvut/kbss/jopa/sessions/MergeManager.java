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

import cz.cvut.kbss.jopa.sessions.change.Change;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.change.UnitOfWorkChangeSet;
import cz.cvut.kbss.jopa.sessions.util.CloneConfiguration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.util.Objects;

/**
 * Merges changes that are made to clones to the registered original objects and live object cache.
 */
class MergeManager {

    private final AbstractUnitOfWork uow;

    private final CloneBuilder builder;

    MergeManager(AbstractUnitOfWork session, CloneBuilder cloneBuilder) {
        this.uow = session;
        this.builder = cloneBuilder;
    }

    private void deleteObjectFromCache(Change changeSet) {
        Object toDelete = changeSet.getOriginal();
        assert toDelete != null;
        uow.removeObjectFromCache(toDelete, changeSet.getEntityContext());
    }

    /**
     * Merge changes from one {@link Change}, which represents the changes made to clone, into the original object.
     *
     * @param changeSet ObjectChangeSet containing changes on a single object
     */
    public void mergeChangesOnObject(ObjectChangeSet changeSet) {
        Objects.requireNonNull(changeSet);
        final Object clone = changeSet.getClone();
        assert clone != null && changeSet.getOriginal() != null;
        builder.mergeChanges(changeSet);
        updateCache(changeSet);
    }

    private void updateCache(ObjectChangeSet changeSet) {
        final Object changedObject = changeSet.getOriginal();
        final Object identifier = EntityPropertiesUtils.getIdentifier(changedObject, uow.getMetamodel());
        boolean preventCaching = changeSet.getChanges().stream().anyMatch(ChangeRecord::doesPreventCaching);
        if (preventCaching) {
            uow.removeObjectFromCache(changedObject, changeSet.getEntityContext());
        } else {
            uow.putObjectIntoCache(identifier, changedObject, changeSet.getDescriptor());
        }
    }

    /**
     * Merge changes from the provided {@link UnitOfWorkChangeSet}.
     *
     * @param changeSet Change set from a single Unit of Work
     */
    public void mergeChangesFromChangeSet(UnitOfWorkChangeSet changeSet) {
        Objects.requireNonNull(changeSet);
        changeSet.getNewObjects().forEach(this::mergeNewObject);
        changeSet.getDeletedObjects().forEach(this::deleteObjectFromCache);
        for (ObjectChangeSet objectChangeSet : changeSet.getExistingObjectsChanges()) {
            mergeChangesOnObject(objectChangeSet);
        }
    }

    /**
     * Merge a newly created object represented by an {@link Change} into the shared live object cache.
     *
     * @param changeSet ObjectChangeSet representing the new object
     */
    public void mergeNewObject(Change changeSet) {
        Objects.requireNonNull(changeSet);
        final Object toCache = builder.buildClone(changeSet.getClone(), CloneConfiguration.withDescriptor(changeSet.getDescriptor()));
        final Object identifier = EntityPropertiesUtils.getIdentifier(toCache, uow.getMetamodel());
        uow.registerOriginalForNewClone(changeSet.getClone(), toCache);
        // Put the original object into the live object cache
        uow.putObjectIntoCache(identifier, toCache, changeSet.getDescriptor());
    }
}
