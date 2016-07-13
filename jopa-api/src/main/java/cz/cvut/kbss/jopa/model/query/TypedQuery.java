/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.query;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.TransactionRequiredException;

import java.net.URI;
import java.util.Collection;
import java.util.List;

public interface TypedQuery<ResultElement> extends Query {
    /**
     * Execute a SELECT query and return the query results as a typed List.
     *
     * @return a list of the results
     * @throws IllegalStateException        if called for a Java Persistence query language UPDATE or DELETE statement
     * @throws TransactionRequiredException if a lock mode has been set and there is no transaction
     * @throws OWLPersistenceException      if the query execution exceeds the query timeout value set and the
     *                                      transaction is rolled back
     */
    @Override
    List<ResultElement> getResultList();

    /**
     * Execute a SELECT query that returns a single result.
     *
     * @return Query result
     * @throws NoResultException       There is no result
     * @throws NoUniqueResultException There are more than one results
     */
    @Override
    ResultElement getSingleResult();

    /**
     * Adds URI of context against which this query will be executed. </p>
     * <p>
     * If no context was specified, the query is run against the default repository context.
     *
     * @param context Context URI
     * @return This instance
     */
    @Override
    TypedQuery<ResultElement> addContext(URI context);

    /**
     * Adds URIs of contexts against which this query will be executed. </p>
     * <p>
     * If no context was specified, the query is run against the default repository context.
     *
     * @param contexts Context URIs
     * @return This instance
     */
    @Override
    TypedQuery<ResultElement> addContexts(Collection<URI> contexts);

    /**
     * Clears the previously set contexts.
     *
     * @return This instance
     * @see #addContext(URI)
     * @see #addContexts(Collection)
     */
    @Override
    TypedQuery<ResultElement> clearContexts();

    /**
     * Set the maximum number of results to retrieve.
     *
     * @param maxResult maximum number of results
     * @return the same query instance
     * @throws IllegalArgumentException if the argument is negative
     */
    @Override
    TypedQuery<ResultElement> setMaxResults(int maxResult);

    /**
     * Binds an argument value to a positional parameter.
     *
     * @param position position
     * @param value    parameter value
     * @return this query instance
     * @throws IllegalArgumentException If position does not correspond to a positional parameter of the query or if the
     *                                  argument is of incorrect type
     */
    @Override
    TypedQuery<ResultElement> setParameter(int position, Object value);

    /**
     * Binds a String argument value to a positional parameter.
     *
     * @param position position
     * @param value    parameter value
     * @param language language tag for the parameter value
     * @return this query instance
     * @throws IllegalArgumentException If position does not correspond to a positional parameter of the query or if the
     *                                  argument is of incorrect type
     */
    @Override
    TypedQuery<ResultElement> setParameter(int position, String value, String language);

    /**
     * Binds an argument value to a named parameter.
     *
     * @param name  parameter name
     * @param value parameter value
     * @return this query instance
     * @throws IllegalArgumentException If the parameter name does not correspond to a parameter of the query or if the
     *                                  argument is of incorrect type
     */
    @Override
    TypedQuery<ResultElement> setParameter(String name, Object value);

    /**
     * Binds a String argument value to a named parameter.
     *
     * @param name     parameter name
     * @param value    parameter value
     * @param language language tag for the parameter value
     * @return this query instance
     * @throws IllegalArgumentException If the parameter name does not correspond to a parameter of the query or if the
     *                                  argument is of incorrect type
     */
    @Override
    TypedQuery<ResultElement> setParameter(String name, String value, String language);

    /**
     * Binds the value of a Parameter object.
     *
     * @param parameter parameter object
     * @param value     parameter value
     * @return this query instance
     * @throws IllegalArgumentException If the parameter does not correspond to a parameter of the query
     */
    @Override
    <T> TypedQuery<ResultElement> setParameter(Parameter<T> parameter, T value);

    /**
     * Binds the value of a String Parameter.
     *
     * @param parameter parameter object
     * @param value     parameter value
     * @param language  language tag for the parameter value
     * @return this query instance
     * @throws IllegalArgumentException If the parameter does not correspond to a parameter of the query
     */
    @Override
    TypedQuery<ResultElement> setParameter(Parameter<String> parameter, String value, String language);
}
