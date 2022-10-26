/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.query;

import cz.cvut.kbss.jopa.exceptions.NoResultException;
import cz.cvut.kbss.jopa.exceptions.NoUniqueResultException;
import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.exceptions.TransactionRequiredException;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

public interface Query {

    /**
     * Execute an update or delete statement.
     *
     * @throws IllegalStateException        if called for a SELECT statement or for a criteria query
     * @throws TransactionRequiredException if there is no transaction or the persistence context has not been joined to
     *                                      the transaction
     * @throws OWLPersistenceException      if the query execution exceeds the query timeout value set and the
     *                                      transaction is rolled back
     */
    void executeUpdate();

    /**
     * Execute a SELECT query and return the query results as an untyped List.
     *
     * @return a list of the results
     * @throws IllegalStateException        if called for a Java Persistence query language UPDATE or DELETE statement
     * @throws TransactionRequiredException if a lock mode has been set and there is no transaction
     * @throws OWLPersistenceException      if the query execution exceeds the query timeout value set and the
     *                                      transaction is rolled back
     */
    List getResultList();

    /**
     * Execute a SELECT query and return the query results as an untyped java.util.stream.Stream.
     * <p>
     * By default this method delegates to getResultList().stream(), however persistence provider may choose to override
     * this method to provide additional capabilities.
     *
     * @return a stream of the results
     * @throws IllegalStateException        if called for a SELECT statement or for a criteria query
     * @throws TransactionRequiredException if there is no transaction or the persistence context has not been joined to
     *                                      the transaction
     * @throws OWLPersistenceException      if the query execution exceeds the query timeout value set and the
     *                                      transaction is rolled back
     */
    default Stream getResultStream() {
        return getResultList().stream();
    }

    /**
     * Execute a SELECT query that returns a single result.
     *
     * @return Query result
     * @throws NoResultException       There is no result
     * @throws NoUniqueResultException There are more than one results
     */
    Object getSingleResult();

    /**
     * Set the maximum number of results to retrieve.
     *
     * @param maxResult maximum number of results
     * @return the same query instance
     * @throws IllegalArgumentException if the argument is negative
     */
    Query setMaxResults(int maxResult);

    /**
     * The maximum number of results the query object was set to retrieve.
     * <p>
     * Returns Integer.MAX_VALUE if {@link #setMaxResults(int)} was not applied to the query object.
     *
     * @return maximum number of results
     */
    int getMaxResults();

    /**
     * Set the position of the first result to retrieve.
     *
     * @param startPosition position of the first result, numbered from 0
     * @return the same query instance
     * @throws IllegalArgumentException If the argument is negative
     */
    Query setFirstResult(int startPosition);

    /**
     * The position of the first result the query object was set to retrieve.
     * <p>
     * Returns 0 if {@code setFirstResult} was not applied to the query object.
     *
     * @return position of the first result
     */
    int getFirstResult();

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
     * Returns a boolean indicating whether a value has been bound to the parameter.
     *
     * @param parameter parameter object
     * @return boolean indicating whether parameter has been bound
     */
    boolean isBound(Parameter<?> parameter);

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
    <T> T getParameterValue(Parameter<T> parameter);

    /**
     * Binds an argument value to a positional parameter.
     * <p>
     * This version exploits the type of the parameter value and maps it to the corresponding XSD datatype (if it
     * exists).
     *
     * @param position position
     * @param value    parameter value
     * @return this query instance
     * @throws IllegalArgumentException If position does not correspond to a positional parameter of the query or if the
     *                                  argument is of incorrect type
     * @see #setUntypedParameter(int, Object)
     */
    Query setParameter(int position, Object value);

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
    Query setParameter(int position, String value, String language);

    /**
     * Binds an argument value to a named parameter.
     * <p>
     * This version exploits the type of the parameter value and maps it to the corresponding XSD datatype (if it
     * exists).
     *
     * @param name  parameter name
     * @param value parameter value
     * @return this query instance
     * @throws IllegalArgumentException If the parameter name does not correspond to a parameter of the query or if the
     *                                  argument is of incorrect type
     * @see #setUntypedParameter(String, Object)
     */
    Query setParameter(String name, Object value);

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
    Query setParameter(String name, String value, String language);

    /**
     * Binds the value of a Parameter object.
     * <p>
     * This version exploits the type of the parameter value and maps it to the corresponding XSD datatype (if it
     * exists).
     *
     * @param parameter parameter object
     * @param value     parameter value
     * @return this query instance
     * @throws IllegalArgumentException If the parameter does not correspond to a parameter of the query
     * @see #setUntypedParameter(Parameter, Object)
     */
    <T> Query setParameter(Parameter<T> parameter, T value);

    /**
     * Binds the value of a String Parameter.
     *
     * @param parameter parameter object
     * @param value     parameter value
     * @param language  language tag for the parameter value
     * @return this query instance
     * @throws IllegalArgumentException If the parameter does not correspond to a parameter of the query
     */
    Query setParameter(Parameter<String> parameter, String value, String language);

    /**
     * Binds an argument value to a positional parameter.
     * <p>
     * This version does not express the type of the value in the query. Instead, it inserts the value directly into the
     * query string. Can be useful e.g. for specifying OFFSET or LIMIT values.
     *
     * @param position position
     * @param value    parameter value
     * @return this query instance
     * @throws IllegalArgumentException If position does not correspond to a positional parameter of the query or if the
     *                                  argument is of incorrect type
     */
    Query setUntypedParameter(int position, Object value);

    /**
     * Binds an argument value to a named parameter.
     * <p>
     * This version does not express the type of the value in the query. Instead, it inserts the value directly into the
     * query string. Can be useful e.g. for specifying OFFSET or LIMIT values.
     *
     * @param name  parameter name
     * @param value parameter value
     * @return this query instance
     * @throws IllegalArgumentException If parameter name does not correspond to a parameter of the query or if the
     *                                  argument is of incorrect type
     */
    Query setUntypedParameter(String name, Object value);

    /**
     * Binds the value of a Parameter object.
     * <p>
     * This version does not express the type of the value in the query. Instead, it inserts the value directly into the
     * query string. Can be useful e.g. for specifying OFFSET or LIMIT values.
     *
     * @param parameter parameter object
     * @param value     parameter value
     * @return this query instance
     * @throws IllegalArgumentException If the parameter does not correspond to a parameter of the query
     */
    <T> Query setUntypedParameter(Parameter<T> parameter, T value);

    /**
     * Sets a query hint.
     * <p>
     * The hints elements may be used to specify query properties and hints. Vendor-specific hints that are not
     * recognized by a provider are silently ignored.
     *
     * @param hintName Name of the query hint
     * @param value    Value of the query hint
     * @return this query instance
     * @throws IllegalArgumentException If the value is not valid for the hint implementation
     */
    Query setHint(String hintName, Object value);

    /**
     * Gets the hints and associated values that are in effect for this query instance.
     *
     * @return Map of query hints with values
     */
    Map<String, Object> getHints();
}
