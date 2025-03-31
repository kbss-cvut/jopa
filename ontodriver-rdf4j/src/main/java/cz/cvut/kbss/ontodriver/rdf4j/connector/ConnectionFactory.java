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
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import org.eclipse.rdf4j.repository.Repository;

public interface ConnectionFactory {

    /**
     * Creates a storage connection.
     *
     * @return New storage connection
     */
    RepoConnection createStorageConnection();

    /**
     * Closes this factory
     *
     * @throws Rdf4jDriverException When storage access error occurs
     */
    void close() throws OntoDriverException;

    /**
     * Whether this factory is open.
     *
     * @return Factory status
     */
    boolean isOpen();

    /**
     * Sets the underlying repository.
     * <p>
     * Note that this functionality is supported only for in-memory stores.
     *
     * @param repository The new repository
     * @throws Rdf4jDriverException In case setting the repository fails
     */
    void setRepository(Repository repository) throws Rdf4jDriverException;
}
