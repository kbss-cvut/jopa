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
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;

import java.util.Map;

public interface PersistenceFactory {

    /**
     * Builds persistence unit and acquires an entity manager instance.
     *
     * @param repositoryName Repository name, used to create physical and logical URI of the storage
     * @param cacheEnabled   Whether to enable second-level cache
     * @param properties     Additional configuration properties
     * @return Entity manager instance
     */
    EntityManager getEntityManager(String repositoryName, boolean cacheEnabled, Map<String, String> properties);
}
