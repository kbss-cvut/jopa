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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;

import java.net.URI;
import java.util.*;

public class ObjectChangeSetImpl implements ObjectChangeSet {

    // The object the changes are bound to
    private final Object changedObject;

    // Reference to the clone
    private final Object cloneObject;

    // A map of attributeName-ChangeRecord pairs to easily find the attributes to change
    private final Map<FieldSpecification<?, ?>, ChangeRecord> attributesToChange = new HashMap<>();

    // Does this change set represent a new object
    private boolean isNew;

    private final Descriptor descriptor;

    public ObjectChangeSetImpl(Object changedObject, Object cloneObject, Descriptor descriptor) {
        this.changedObject = Objects.requireNonNull(changedObject);
        this.cloneObject = Objects.requireNonNull(cloneObject);
        this.descriptor = Objects.requireNonNull(descriptor);
    }

    @Override
    public void addChangeRecord(ChangeRecord record) {
        Objects.requireNonNull(record);
        attributesToChange.put(record.getAttribute(), record);
    }

    @Override
    public Set<ChangeRecord> getChanges() {
        return new HashSet<>(attributesToChange.values());
    }

    @Override
    public boolean hasChanges() {
        return !attributesToChange.isEmpty();
    }

    @Override
    public Class<?> getObjectClass() {
        return cloneObject.getClass();
    }

    @Override
    public Object getChangedObject() {
        return changedObject;
    }

    @Override
    public Object getCloneObject() {
        return cloneObject;
    }

    @Override
    public void setNew(boolean isNew) {
        this.isNew = isNew;
    }

    @Override
    public boolean isNew() {
        return isNew;
    }

    @Override
    public URI getEntityContext() {
        return descriptor.getSingleContext().orElse(null);
    }

    @Override
    public Descriptor getEntityDescriptor() {
        return descriptor;
    }
}
