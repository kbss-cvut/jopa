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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.CacheManager;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.utils.Configuration;

import java.util.Objects;

/**
 * Defines common session-related methods.
 */
abstract class AbstractSession implements MetamodelProvider, ConfigurationHolder {

    protected Configuration configuration;

    protected AbstractSession(Configuration configuration) {
        this.configuration = Objects.requireNonNull(configuration);
    }

    @Override
    public Configuration getConfiguration() {
        return configuration;
    }

    /**
     * Get the current live object cache.
     * <p>
     * This manager represents the second level cache.
     *
     * @return Second level cache manager
     */
    public abstract CacheManager getLiveObjectCache();

    /**
     * Acquires connection to the underlying ontology storage.
     *
     * @return Connection
     */
    protected abstract ConnectionWrapper acquireConnection();

    /**
     * Gets a {@link CriteriaBuilder} instance for building Criteria API queries.
     *
     * @return Criteria query builder
     */
    public abstract CriteriaBuilder getCriteriaBuilder();
}
