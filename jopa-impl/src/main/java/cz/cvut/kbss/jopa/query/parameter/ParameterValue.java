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
package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.query.sparql.SparqlConstants;

import java.util.ArrayList;
import java.util.List;

/**
 * Query parameter value holder.
 */
public interface ParameterValue {

    /**
     * Gets the value held by this wrapper.
     *
     * @return The parameter value
     */
    Object getValue();

    /**
     * Gets this parameter value as a string which can be inserted directly into a query.
     *
     * @return Value as query string
     */
    String getQueryString();

    /**
     * Builds a list of the specified size containing the value(s) represented by this parameter value.
     * <p>
     * If this instance does not contain enough values to fill in the list of the specified size, its remainder is
     * filled with {@link cz.cvut.kbss.jopa.query.sparql.SparqlConstants#UNDEF}s.
     * <p>
     * The resulting list will be used to build a SPARQL {@literal VALUES} table.
     *
     * @param size Requested size of value list
     * @return List of values
     */
    default List<String> toQueryValues(int size) {
        assert size > 0;

        if (size == 1) {
            return List.of(getQueryString());
        }
        final List<String> result = new ArrayList<>(size);
        result.add(getQueryString());
        for (int i = 1; i < size; i++) {
            result.add(SparqlConstants.UNDEF);
        }
        return result;
    }

    /**
     * Whether this parameter value is set or it represents just the parameter identification.
     *
     * @return {@code true} if this instance represents an explicit parameter value
     */
    default boolean isSet() {
        return true;
    }

    /**
     * Returns the number of values held by this instance.
     * <p>
     * Will return number different from 1 only for collection value parameters.
     *
     * @return Number of values represented by this instance
     */
    default int valueCount() {return 1;}
}
