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
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.sessions.UnitOfWork;

import java.util.*;

/**
 * A set of changes made in a {@link UnitOfWork}.
 */
public class UnitOfWorkChangeSet {

    private final Set<DeleteObjectChange> deletedObjects;
    private final Map<Object, ObjectChangeSet> objectChanges;
    private final Set<NewObjectChange> newObjectChanges;

    public UnitOfWorkChangeSet() {
        this.objectChanges = new HashMap<>();
        this.deletedObjects = new HashSet<>();
        this.newObjectChanges = new HashSet<>();
    }

    /**
     * Add new ObjectChangeSet to this changeSet.
     *
     * @param objectChangeSet ObjectChangeSet
     */
    public void addObjectChangeSet(ObjectChangeSet objectChangeSet) {
        objectChanges.put(objectChangeSet.getOriginal(), objectChangeSet);
    }

    /**
     * Adds a change set for deleted object.
     *
     * @param deletedObject The change set to add
     */
    public void addDeletedObjectChangeSet(DeleteObjectChange deletedObject) {
        deletedObjects.add(deletedObject);
    }

    /**
     * Add a change set for newly created object. These changes are held in separate attribute and get special treatment
     * when merged into shared session cache.
     *
     * @param newObject ObjectChangeSet
     */
    public void addNewObjectChangeSet(NewObjectChange newObject) {
        newObjectChanges.add(newObject);
    }

    /**
     * Returns change sets for existing modified objects.
     * <p>
     * New object and deleted object change sets are not included.
     *
     * @return Collection of change sets
     */
    public Collection<ObjectChangeSet> getExistingObjectsChanges() {
        return Collections.unmodifiableCollection(objectChanges.values());
    }

    /**
     * Removes change record of the specified original object, if present, cancelling the changes.
     *
     * @param original The object whose changes should be removed
     */
    public void cancelObjectChanges(Object original) {
        objectChanges.remove(original);
    }

    /**
     * Gets changes for the specified original object (if there are any).
     *
     * @param original The object for which changes should be found
     * @return Object change set or null, if the object has no changes
     */
    public ObjectChangeSet getExistingObjectChanges(Object original) {
        return objectChanges.get(original);
    }

    /**
     * Returns the collection of deleted objects.
     *
     * @return Set of change sets
     */
    public Set<DeleteObjectChange> getDeletedObjects() {
        return this.deletedObjects;
    }

    /**
     * Returns the collection of change sets for newly created objects.
     *
     * @return Set of change sets
     */
    public Set<NewObjectChange> getNewObjects() {
        return this.newObjectChanges;
    }

    /**
     * Returns true if there are deleted objects in this change set.
     *
     * @return boolean
     */
    public boolean hasDeleted() {
        return !deletedObjects.isEmpty();
    }

    /**
     * Returns true if this changeSet has any changes.
     *
     * @return boolean
     */
    public boolean hasChanges() {
        return hasDeleted() || hasNew() || !objectChanges.isEmpty();
    }

    /**
     * Are there any new objects in the change set?
     *
     * @return boolean
     */
    public boolean hasNew() {
        return !this.newObjectChanges.isEmpty();
    }
}
