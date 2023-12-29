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
package cz.cvut.kbss.jopa.model;

import java.util.List;

/**
 * Determine the list of persistence providers available in the runtime
 * environment.
 * <p>
 * Implementations must be thread-safe.
 * <p>
 * Note that the getPersistenceProviders method can potentially be called many
 * times: it is recommended that the implementation of this method make use of
 * caching.
 *
 * @see PersistenceProvider
 */
public interface PersistenceProviderResolver {

    /**
     * Returns a list of the PersistenceProvider implementations available in
     * the runtime environment.
     *
     * @return list of the persistence providers available in the environment
     */
    List<PersistenceProvider> getPersistenceProviders();

    /**
     * Clear cache of providers
     */
    void clearCachedProviders();
}
