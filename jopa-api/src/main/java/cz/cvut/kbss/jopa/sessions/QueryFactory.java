/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.query.Query;
import cz.cvut.kbss.jopa.model.query.TypedQuery;

public interface QueryFactory {

    /**
     * Creates query object representing a native SPARQL query. </p>
     *
     * @param sparql The query
     * @return Query object
     * @throws NullPointerException If {@code sparql} is {@code null}
     */
    Query createNativeQuery(String sparql);

    /**
     * Creates typed query object representing a native SPARQL query. </p>
     *
     * @param sparql      The query
     * @param resultClass Type of the results
     * @return Query object
     * @throws NullPointerException If {@code sparql} or {@code resultClass} is {@code null}
     */
    <T> TypedQuery<T> createNativeQuery(String sparql, Class<T> resultClass);

    /**
     * Creates query object representing a native SPARQL query. </p>
     *
     * @param query The query
     * @return Query object
     * @throws NullPointerException If {@code sparql} is {@code null}
     */
    Query createQuery(String query);

    /**
     * Creates typed query object representing a native SPARQL query. </p>
     *
     * @param query       The query
     * @param resultClass Type of the results param URI of the ontology context against which the query will be
     *                    evaluated
     * @return Query object
     * @throws NullPointerException If {@code sparql} or {@code resultClass} is {@code null}
     */
    <T> TypedQuery<T> createQuery(String query, Class<T> resultClass);
}
