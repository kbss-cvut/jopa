/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.sessions.change.ChangeRecord;

@FunctionalInterface
public interface ValueMerger {

    /**
     * Merges new value specified by the change record into the target object.
     * <p>
     * Note that the change record may be modified during the merge (e.g., in case the new value is a detached instance of a managed type and
     * a corresponding object needs to be loaded from the underlying repository).
     *
     * @param target              Target of the merge
     * @param changeRecord        Description of the change (including the new value)
     * @param attributeDescriptor Specifies context of the merged attribute
     */
    void mergeValue(Object target, ChangeRecord changeRecord, Descriptor attributeDescriptor);
}
