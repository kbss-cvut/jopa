/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.net.URI;
import java.util.Set;

public interface ObjectChangeSet {

    /**
     * Adds a new change record to this change set.
     * <p>
     * If there was a change for attribute represented by the new record, it will be overwritten.
     *
     * @param record The record to add
     */
    void addChangeRecord(ChangeRecord record);

    /**
     * Gets type of the changed object.
     *
     * @return Object type
     */
    Class<?> getObjectClass();

    /**
     * Gets changes held in this change set.
     *
     * @return Set of changes
     */
    Set<ChangeRecord> getChanges();

    /**
     * Whether this change set contains an changes.
     *
     * @return {@code true} if there are any changes in this change set, {@code false} otherwise
     */
    boolean hasChanges();

    /**
     * Specifies whether this change set represents a new object.
     *
     * @param isNew Whether this is a new object's change set
     */
    void setNew(boolean isNew);

    /**
     * Whether this is a new object's change set.
     *
     * @return Whether target object is new
     */
    boolean isNew();

    /**
     * Gets the clone with changes.
     *
     * @return Clone
     */
    Object getCloneObject();

    /**
     * Gets the original object.
     *
     * @return Original
     */
    Object getChangedObject();

    /**
     * Gets descriptor of the changed object.
     *
     * @return Instance descriptor
     */
    Descriptor getEntityDescriptor();

    /**
     * Gets ontology context URI, to which the changed object belongs.
     *
     * @return context URI
     */
    URI getEntityContext();
}
