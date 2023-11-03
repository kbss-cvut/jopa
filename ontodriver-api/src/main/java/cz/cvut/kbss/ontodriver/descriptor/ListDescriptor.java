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
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;

/**
 * This interface declares the basic methods for working with sequences in JOPA.
 */
public interface ListDescriptor {

    /**
     * Gets context in which the list is stored.
     *
     * @return Context URI, {@code null} if it is not set and the default is assumed
     */
    URI getContext();

    /**
     * Sets context of the list.
     *
     * @param context Context URI, can be {@code null}
     */
    void setContext(URI context);

    /**
     * Gets owner of the list.
     * <p>
     * That is, the named resource which is at the head of the list. In object
     * model, it is the owning entity.
     *
     * @return List owner
     */
    NamedResource getListOwner();

    /**
     * Gets the property assertion which connects the list to its owner.
     *
     * @return Property assertion
     * @see #getListOwner()
     */
    Assertion getListProperty();

    /**
     * Gets the property assertion which connects the list nodes to each other.
     *
     * @return Property assertion
     */
    Assertion getNextNode();
}