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

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;

import java.util.*;

public class ObjectChangeSet implements Change {

    // The object the changes are bound to
    private final Object changedObject;

    // Reference to the clone
    private final Object cloneObject;

    // A map of attributeName-ChangeRecord pairs to easily find the attributes to change
    private final Map<FieldSpecification<?, ?>, ChangeRecord> attributesToChange = new HashMap<>();

    private final Descriptor descriptor;

    public ObjectChangeSet(Object changedObject, Object cloneObject, Descriptor descriptor) {
        this.changedObject = Objects.requireNonNull(changedObject);
        this.cloneObject = Objects.requireNonNull(cloneObject);
        this.descriptor = Objects.requireNonNull(descriptor);
    }

    /**
     * Adds a new change record to this change set.
     * <p>
     * If there was a change for attribute represented by the new record, it will be overwritten.
     *
     * @param record The record to add
     */
    public void addChangeRecord(ChangeRecord record) {
        Objects.requireNonNull(record);
        attributesToChange.put(record.getAttribute(), record);
    }

    /**
     * Gets changes held in this change set.
     *
     * @return Set of changes
     */
    public Set<ChangeRecord> getChanges() {
        return new HashSet<>(attributesToChange.values());
    }

    /**
     * Whether this change set contains any changes.
     *
     * @return {@code true} if there are any changes in this change set, {@code false} otherwise
     */
    public boolean hasChanges() {
        return !attributesToChange.isEmpty();
    }

    @Override
    public Object getOriginal() {
        return changedObject;
    }

    @Override
    public Object getClone() {
        return cloneObject;
    }

    @Override
    public Descriptor getDescriptor() {
        return descriptor;
    }
}
