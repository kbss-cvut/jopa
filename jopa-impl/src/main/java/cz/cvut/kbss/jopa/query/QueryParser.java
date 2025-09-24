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
package cz.cvut.kbss.jopa.query;

/**
 * Used to parse queries into builders which enable the query to be further manipulated, e.g., set parameters.
 */
public interface QueryParser {

    /**
     * Parses the specified query string and returns a query holder instance containing the parsed query.
     *
     * @param query The query to parse
     * @return Query holder with the parsed query
     */
    QueryHolder parseQuery(String query);

    /**
     * Parses the specified query string and returns a query holder instance containing the parsed query.
     * <p>
     * The provided result class can be used when processing the query to make adjustments.
     *
     * @param query       Query to parse
     * @param resultClass Result class provided by the client
     * @return Query holder with the parsed query
     */
    default QueryHolder parseQuery(String query, Class<?> resultClass) {
        return parseQuery(query);
    }
}
