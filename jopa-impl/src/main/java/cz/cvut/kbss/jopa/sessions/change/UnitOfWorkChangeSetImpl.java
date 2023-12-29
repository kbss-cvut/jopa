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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkChangeSet;

import java.util.*;

public class UnitOfWorkChangeSetImpl implements UnitOfWorkChangeSet {

    private final Set<ObjectChangeSet> deletedObjects;
    private final Map<Object, ObjectChangeSet> objectChanges;
    private final Set<ObjectChangeSet> newObjectChanges;

    public UnitOfWorkChangeSetImpl() {
        this.objectChanges = new HashMap<>();
        this.deletedObjects = new HashSet<>();
        this.newObjectChanges = new HashSet<>();
    }

    @Override
    public void addObjectChangeSet(ObjectChangeSet objectChangeSet) {
        if (objectChangeSet.isNew()) {
            addNewObjectChangeSet(objectChangeSet);
        } else {
            objectChanges.put(objectChangeSet.getChangedObject(), objectChangeSet);
        }
    }

    @Override
    public void addDeletedObjectChangeSet(ObjectChangeSet deletedObject) {
        deletedObjects.add(deletedObject);
    }

    @Override
    public void addNewObjectChangeSet(ObjectChangeSet newObject) {
        newObject.setNew(true);
        newObjectChanges.add(newObject);
    }

    @Override
    public Collection<ObjectChangeSet> getExistingObjectsChanges() {
        return Collections.unmodifiableCollection(objectChanges.values());
    }

    @Override
    public void cancelObjectChanges(Object original) {
        objectChanges.remove(original);
    }

    @Override
    public ObjectChangeSet getExistingObjectChanges(Object original) {
        return objectChanges.get(original);
    }

    @Override
    public Set<ObjectChangeSet> getDeletedObjects() {
        return this.deletedObjects;
    }

    @Override
    public Set<ObjectChangeSet> getNewObjects() {
        return this.newObjectChanges;
    }

    @Override
    public boolean hasDeleted() {
        return !deletedObjects.isEmpty();
    }

    @Override
    public boolean hasChanges() {
        return hasDeleted() || hasNew() || !objectChanges.isEmpty();
    }

    @Override
    public boolean hasNew() {
        return !this.newObjectChanges.isEmpty();
    }
}
