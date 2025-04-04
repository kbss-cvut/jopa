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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;
import cz.cvut.kbss.jopa.sessions.change.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.merge.DetachedValueMerger;
import cz.cvut.kbss.jopa.sessions.merge.ValueMerger;

class DetachedInstanceMerger {

    private final ValueMerger valueMerger;

    DetachedInstanceMerger(UnitOfWork uow) {
        this.valueMerger = new DetachedValueMerger(uow);
    }

    /**
     * Merges changes from the detached instance being merged into persistence context (clone in the specified change
     * set) into the corresponding managed instance (original in the change set).
     *
     * @param changeSet Set of changes to apply to the managed instance
     * @return Managed instance with changes merged into it
     */
    Object mergeChangesFromDetachedToManagedInstance(ObjectChangeSet changeSet, Descriptor descriptor) {
        assert changeSet != null;
        assert changeSet.getClone() != null;
        final Object target = changeSet.getOriginal();
        assert target != null;

        for (ChangeRecord change : changeSet.getChanges()) {
            valueMerger.mergeValue(target, change, descriptor.getAttributeDescriptor(change.getAttribute()));
        }
        return target;
    }
}
