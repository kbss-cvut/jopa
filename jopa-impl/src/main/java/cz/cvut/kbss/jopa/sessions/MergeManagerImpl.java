/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.util.Objects;

public class MergeManagerImpl implements MergeManager {

    protected UnitOfWorkImpl uow;

    protected CloneBuilder builder;

    MergeManagerImpl(UnitOfWorkImpl session) {
        this.uow = session;
        this.builder = session.getCloneBuilder();
    }

    private void deleteObjectFromCache(ObjectChangeSet changeSet) {
        Object toDelete = changeSet.getChangedObject();
        assert toDelete != null;
        uow.removeObjectFromCache(toDelete, changeSet.getEntityContext());
    }

    @Override
    public Object mergeChangesOnObject(ObjectChangeSet changeSet) {
        Objects.requireNonNull(changeSet);
        final Object clone = changeSet.getCloneObject();
        if (clone == null) {
            return null;
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
        return clone;
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

    @Override
    public void mergeChangesFromChangeSet(UnitOfWorkChangeSet changeSet) {
        Objects.requireNonNull(changeSet);
        for (ObjectChangeSet objectChangeSet : changeSet.getExistingObjectsChanges()) {
            mergeChangesOnObject(objectChangeSet);
        }
        changeSet.getNewObjects().forEach(this::mergeNewObject);
        changeSet.getDeletedObjects().forEach(this::deleteObjectFromCache);

    }

    @Override
    public void mergeNewObject(ObjectChangeSet changeSet) {
        Objects.requireNonNull(changeSet);
        if (!changeSet.isNew()) {
            mergeChangesOnObject(changeSet);
            return;
        }
        // Put the original object into the shared session cache
        updateCache(changeSet);
    }
}
