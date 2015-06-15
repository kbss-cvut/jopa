/**
 * Copyright (C) 2011 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package cz.cvut.kbss.jopa.model.query;

import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.TransactionRequiredException;

import javax.xml.transform.Result;

public interface Query<ResultElement> {
    /**
     * Execute a SELECT query and return the query results as an untyped List.
     *
     * @return a list of the results
     * @throws IllegalStateException        if called for a Java Persistence query language UPDATE or
     *                                      DELETE statement
     * @throws QueryTimeoutException        if the query execution exceeds the query timeout value set
     *                                      and only the statement is rolled back
     * @throws TransactionRequiredException if a lock mode has been set and there is no transaction
     * @throws PessimisticLockException     if pessimistic locking fails and the transaction is rolled
     *                                      back
     * @throws LockTimeoutException         if pessimistic locking fails and only the statement is rolled
     *                                      back
     * @throws OWLPersistenceException      if the query execution exceeds the query timeout value set
     *                                      and the transaction is rolled back
     */
    List<ResultElement> getResultList();

    /**
     * Execute a SELECT query that returns a single result.
     *
     * @return Query result
     * @throws NoResultException       There is no result
     * @throws NoUniqueResultException There are more than one results
     */
    ResultElement getSingleResult();

    /**
     * Adds URI of context against which this query will be executed. </p>
     * <p>
     * If no context was specified, the query is run against the default
     * repository context.
     *
     * @param context Context URI
     * @return This instance
     */
    Query<ResultElement> addContext(URI context);

    /**
     * Adds URIs of contexts against which this query will be executed. </p>
     * <p>
     * If no context was specified, the query is run against the default
     * repository context.
     *
     * @param contexts Context URIs
     * @return This instance
     */
    Query<ResultElement> addContexts(Collection<URI> contexts);

    /**
     * Clears the previously set contexts.
     *
     * @return This instance
     * @see #addContext(URI)
     * @see #addContexts(Collection)
     */
    Query<ResultElement> clearContexts();

    /**
     * Set the maximum number of results to retrieve.
     *
     * @param maxResult maximum number of results
     * @return the same query instance
     * @throws IllegalArgumentException if the argument is negative
     */
    Query<ResultElement> setMaxResults(int maxResult);

    /**
     * The maximum number of results the query object was set to retrieve. </p>
     * <p>
     * Returns Integer.MAX_VALUE if {@link #setMaxResults(int)} was not applied
     * to the query object.
     *
     * @return maximum number of results
     */
    int getMaxResults();

    /**
     * Gets the parameter object corresponding to the declared positional parameter with the given position. This method
     * is not required to be supported for native queries.
     *
     * @param position position
     * @return parameter object
     * @throws IllegalArgumentException If the parameter with the specified position does not exist
     */
    Parameter<?> getParameter(int position);

    /**
     * Gets the parameter object corresponding to the declared parameter of the given name. This method is not required
     * to be supported for native queries.
     *
     * @param name Parameter name
     * @return parameter object
     * @throws IllegalArgumentException If the parameter of the specified name does not exist
     */
    Parameter<?> getParameter(String name);

    /**
     * Gets the parameter objects corresponding to the declared parameters of the query. Returns empty set if the query
     * has no parameters. This method is not required to be supported for native queries.
     *
     * @return set of parameter objects
     */
    Set<Parameter<?>> getParameters();

    /**
     * Returns the input value bound to the positional parameter.
     *
     * @param position position
     * @return parameter value
     * @throws IllegalStateException    If the parameter has not been bound
     * @throws IllegalArgumentException If the parameter with the specified position does not exist
     */
    Object getParameterValue(int position);

    /**
     * Returns the input value bound to the named parameter.
     *
     * @param name parameter name
     * @return parameter value
     * @throws IllegalStateException    If the parameter has not been bound
     * @throws IllegalArgumentException If the parameter with the specified name does not exist
     */
    Object getParameterValue(String name);

    /**
     * Returns the input value bound to the parameter.
     *
     * @param parameter parameter object
     * @return parameter value
     * @throws IllegalStateException    If the parameter has not been bound
     * @throws IllegalArgumentException If the parameter is not a parameter of the query
     */
    Object getParameterValue(Parameter<?> parameter);

    /**
     * Binds an argument value to a positional parameter.
     *
     * @param position position
     * @param value    parameter value
     * @return this query instance
     * @throws IllegalArgumentException If position does not correspond to a positional parameter of the query or if the
     *                                  argument is of incorrect type
     */
    Query<ResultElement> setParameter(int position, Object value);

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
    Query<ResultElement> setParameter(int position, String value, String language);

    /**
     * Binds an argument value to a named parameter.
     *
     * @param name  parameter name
     * @param value parameter value
     * @return this query instance
     * @throws IllegalArgumentException If the parameter name does not correspond to a parameter of the query or if the
     *                                  argument is of incorrect type
     */
    Query<ResultElement> setParameter(String name, Object value);

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
    Query<ResultElement> setParameter(String name, String value, String language);
}
