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
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;

/**
 * Describes storage of a referenced list.
 * <p>
 * A referenced list consists of nodes representing the list structure and values to which each node points.
 */
public interface ReferencedListDescriptor extends ListDescriptor {

    /**
     * Gets the property assertion which represents each node's content.
     *
     * @return Property assertion
     */
    Assertion getNodeContent();

    /**
     * Whether the list is terminated by {@literal rdf:nil}.
     *
     * @return {@code true} if the list is terminated by nil, {@code false} otherwise
     */
    boolean isTerminatedByNil();
}
