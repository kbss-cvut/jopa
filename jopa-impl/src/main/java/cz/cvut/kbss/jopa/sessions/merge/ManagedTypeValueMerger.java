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
package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

class ManagedTypeValueMerger implements ValueMerger {

    private final UnitOfWork uow;

    ManagedTypeValueMerger(UnitOfWork uow) {
        this.uow = uow;
    }

    @Override
    public void mergeValue(Object target, ChangeRecord changeRecord, Descriptor attributeDescriptor) {
        final Object mergedValue = changeRecord.getNewValue();
        final Object toSet = getValueToSet(mergedValue, attributeDescriptor);
        EntityPropertiesUtils.setFieldValue(changeRecord.getAttribute().getJavaField(), target, toSet);
        // Replace the value in the change record as the mergedValue may not have been managed
        changeRecord.setNewValue(toSet);
    }

    Object getValueToSet(Object mergedValue, Descriptor descriptor) {
        if (mergedValue == null) {
            return null;
        }
        if (uow.contains(mergedValue)) {
            return mergedValue;
        }
        final Object identifier = EntityPropertiesUtils.getIdentifier(mergedValue, uow.getMetamodel());
        if (identifier == null) {
            return mergedValue;
        }
        final Class<?> type = mergedValue.getClass();
        final Object managedInstance = uow.readObject(type, identifier, descriptor);
        // If the object cannot be found, it is a new one (not yet registered), which we can assign directly
        return managedInstance != null ? managedInstance : mergedValue;
    }
}
