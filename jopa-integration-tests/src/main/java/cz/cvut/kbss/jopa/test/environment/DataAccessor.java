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
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;

import java.util.Collection;

public interface DataAccessor {

    /**
     * Persists the specified test data directly into the storage.
     * <p>
     * Data are persisted using a vendor-specific storage access object unwrapped from the specified entity manager.
     *
     * @param data Data to persist
     * @param em   Means of getting vendor-specific storage access
     * @throws Exception If storage access error occurs
     */
    void persistTestData(Collection<Quad> data, EntityManager em) throws Exception;

    /**
     * Verifies that the specified data are present in the storage.
     * <p>
     * Data presence is verified using a vendor-specific storage access object unwrapped from the specified entity
     * manager.
     *
     * @param data Data to verify
     * @param em   Means of getting vendor-specific storage access
     * @throws Exception If storage access error occurs
     */
    void verifyDataPresent(Collection<Quad> data, EntityManager em) throws Exception;

    /**
     * Verifies that the specified data are not present in the storage.
     * <p>
     * Data presence is verified using a vendor-specific storage access object unwrapped from the specified entity
     * manager.
     *
     * @param data Data to verify
     * @param em   Means of getting vendor-specific storage access
     * @throws Exception If storage access error occurs
     */
    void verifyDataNotPresent(Collection<Quad> data, EntityManager em) throws Exception;

    /**
     * Exports the contents of the repository managed by the specified entity manager.
     *
     * @param em EntityManager instance
     * @return String representing the contents of the repository serialized to RDF
     */
    String exportRepository(EntityManager em);
}
