/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
}
