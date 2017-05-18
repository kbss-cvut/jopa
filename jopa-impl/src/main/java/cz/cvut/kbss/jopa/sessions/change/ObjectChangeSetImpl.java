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

import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;

import java.net.URI;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ObjectChangeSetImpl implements ObjectChangeSet {

    // Class this ObjectChangeSet represents
    private Class<?> objectClass;

    // The object the changes are bound to
    private Object changedObject;

    // Reference to the clone
    private Object cloneObject;

    // A map of attributeName-ChangeRecord pairs to easily find the attributes to change
    private final Map<FieldSpecification<?, ?>, ChangeRecord> attributesToChange = new HashMap<>();

    // Does this change set represent a new object
    private boolean isNew;

    private URI context;

    public ObjectChangeSetImpl(Object changedObject, Object cloneObject, URI context) {
        this.changedObject = changedObject;
        this.cloneObject = cloneObject;
        this.objectClass = cloneObject.getClass();
        this.context = context;
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

    public void setCloneObject(Object cloneObject) {
        this.cloneObject = cloneObject;
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
        return context;
    }
}
