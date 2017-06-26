/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;

import java.net.URI;
import java.util.*;

public class ObjectChangeSetImpl implements ObjectChangeSet {

    // Type of the object represented by this change set
    private final Class<?> objectClass;

    // The object the changes are bound to
    private final Object changedObject;

    // Reference to the clone
    private final Object cloneObject;

    // A map of attributeName-ChangeRecord pairs to easily find the attributes to change
    private final Map<FieldSpecification<?, ?>, ChangeRecord> attributesToChange = new HashMap<>();

    // Does this change set represent a new object
    private boolean isNew;

    private Descriptor descriptor;

    public ObjectChangeSetImpl(Object changedObject, Object cloneObject, Descriptor descriptor) {
        this.changedObject = Objects.requireNonNull(changedObject);
        this.cloneObject = Objects.requireNonNull(cloneObject);
        this.objectClass = cloneObject.getClass();
        this.descriptor = Objects.requireNonNull(descriptor);
    }

    @Override
    public void addChangeRecord(ChangeRecord record) {
        if (record == null)
            return;
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
        return objectClass;
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
        return descriptor.getContext();
    }

    @Override
    public Descriptor getEntityDescriptor() {
        return descriptor;
    }
}
