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

import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;

public interface QueryFactory {

    /**
     * Creates query object representing a native SPARQL query.
     *
     * @param sparql The query
     * @return Query object
     * @throws NullPointerException If {@code sparql} is {@code null}
     */
    Query createNativeQuery(String sparql);

    /**
     * Creates typed query object representing a native SPARQL query.
     *
     * @param sparql      The query
     * @param resultClass Type of the results
     * @return Query object
     * @throws NullPointerException If {@code sparql} or {@code resultClass} is {@code null}
     */
    <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass);

    /**
     * Creates a query object representing a native SPARQL query.
     * @param sparql The query
     * @param resultSetMapping Name of the result set mapping to apply
     * @return Query object
     * * @throws NullPointerException If {@code sparql} or {@code resultSetMapping} is {@code null}
     */
    Query createNativeQuery(String sparql, String resultSetMapping);

    /**
     * Creates query object representing a native SPARQL query.
     *
     * @param query The query
     * @return Query object
     * @throws NullPointerException If {@code sparql} is {@code null}
     */
    Query createQuery(String query);

    /**
     * Creates typed query object representing a native SPARQL query.
     *
     * @param query       The query
     * @param resultClass Type of the results param URI of the ontology context against which the query will be
     *                    evaluated
     * @return Query object
     * @throws NullPointerException If {@code sparql} or {@code resultClass} is {@code null}
     */
    <T> TypedQuery<T> createQuery(String query, Class<T> resultClass);

    /**
     * Creates a query object representing a native SPARQL query.
     *
     * @param name The name of the query defined in metadata
     * @return Query object
     * @throws IllegalArgumentException If a query has not been defined with the given name
     */
    Query createNamedQuery(String name);

    /**
     * Creates a typed query object representing a native SPARQL query.
     *
     * @param name        The name of the query defined in metadata
     * @param resultClass Type of the results param URI of the ontology context against which the query will be
     *                    evaluated
     * @return Query object
     * @throws IllegalArgumentException If a query has not been defined with the given name
     */
    <T> TypedQuery<T> createNamedQuery(String name, Class<T> resultClass);
}
