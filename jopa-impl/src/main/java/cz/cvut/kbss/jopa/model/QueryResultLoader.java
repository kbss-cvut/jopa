/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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

import cz.cvut.kbss.ontodriver.iteration.ResultRow;

import java.util.Optional;

/**
 * Loads result from a query result row.
 *
 * @param <T> Type of the result
 */
public interface QueryResultLoader<T> {

    /**
     * Loads the result from the given result row.
     *
     * @param resultRow Result row to load from
     * @return Loaded value, if present
     */
    Optional<T> loadResult(ResultRow resultRow);

    /**
     * If the loader performs any aggregation, use this to method to get the last pending result.
     *
     * @return Last pending value
     */
    default Optional<T> loadLastPending() {
        return Optional.empty();
    }
}
