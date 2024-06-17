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
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * Interface for closeable resources.
 */
public interface Closeable {

    /**
     * Closes this resource releasing any sub-resources it holds.
     * <p>
     * After closing the resource is not usable any more and calling methods on
     * it (except {@code close} and {@code isOpen}) will result in
     * {@code IllegalStateException}.
     * <p>
     * Calling {@code close} on already closed resource does nothing.
     *
     * @throws OntoDriverException If an ontology access error occurs.
     */
    void close() throws OntoDriverException;

    /**
     * Retrieves status of this resource.
     *
     * @return {@code true} if the resource is open, {@code false} otherwise
     */
    boolean isOpen();

}
