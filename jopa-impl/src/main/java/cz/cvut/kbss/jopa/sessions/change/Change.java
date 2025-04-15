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
import cz.cvut.kbss.jopa.utils.MetamodelUtils;

import java.net.URI;

/**
 * Represents a change to an object made during a transaction.
 */
public interface Change {

    /**
     * Gets the type of the changed object.
     *
     * @return Object type
     */
    default Class<?> getObjectClass() {
        return MetamodelUtils.getEntityClass(getClone().getClass());
    }

    /**
     * Gets the clone with changes.
     *
     * @return Clone, never {@code null}
     */
    Object getClone();

    /**
     * Gets the original object.
     *
     * @return Original
     */
    Object getOriginal();

    /**
     * Gets descriptor of the changed object.
     *
     * @return Instance descriptor
     */
    Descriptor getDescriptor();

    /**
     * Gets identifier of the repository context to which the changed object belongs.
     *
     * @return context URI
     */
    default URI getEntityContext() {
        return getDescriptor().getSingleContext().orElse(null);
    }
}
