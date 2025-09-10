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

import cz.cvut.kbss.jopa.model.query.Parameter;

import java.util.Set;

/**
 * Represents a caretaker of a query, enabling parameter setting and final assembly of the query.
 */
public interface QueryHolder {

    /**
     * Gets the original query string.
     *
     * @return Gets the original unparsed query
     */
    String getQuery();

    /**
     * Gets a collection of parameters in the query.
     *
     * @return Parameter names
     */
    Set<Parameter<?>> getParameters();

    /**
     * Checks whether the query has a parameter with the specified name.
     *
     * @param name Parameter name
     * @return {@code true} if the query has a matching parameter, {@code false} otherwise
     */
    boolean hasParameter(String name);

    /**
     * Checks whether the query has a parameter at the specified position.
     *
     * @param position Parameter position
     * @return {@code true} if the query has a matching parameter, {@code false} otherwise
     */
    boolean hasParameter(int position);

    /**
     * Gets a parameter with the specified name.
     *
     * @param name Parameter name
     * @return Parameter object
     * @throws IllegalArgumentException If the parameter of the specified name does not exist
     */
    Parameter<?> getParameter(String name);

    /**
     * Gets a parameter with the specified position.
     *
     * @param position Parameter position
     * @return Parameter object
     * @throws IllegalArgumentException If the parameter at the specified position does not exist
     */
    Parameter<?> getParameter(int position);

    /**
     * Gets value bound to the specified parameter.
     *
     * @param parameter Parameter
     * @return parameter value
     * @throws IllegalArgumentException If there is no parameter with the specified name
     */
    Object getParameterValue(Parameter<?> parameter);

    /**
     * Sets value of the specified parameter in the query.
     * <p>
     * If a value was already specified for the parameter, it is overwritten by the new one. This version expresses the
     * value type information using XSD datatype in the query string.
     *
     * @param parameter Parameter object
     * @param value     Value to use
     * @param <T>       Type of the parameter
     * @throws IllegalArgumentException If there is no such parameter in the query
     * @see #setUntypedParameter(Parameter, Object)
     */
    <T> void setParameter(Parameter<T> parameter, Object value);

    /**
     * Sets value of the specified parameter in the query.
     * <p>
     * If a value was already specified for the parameter, it is overwritten by the new one.
     *
     * @param parameter Parameter object
     * @param value     String value to use
     * @param language  Parameter language
     * @param <T>       Type of the parameter
     * @throws IllegalArgumentException If there is no such parameter in the query
     */
    <T> void setParameter(Parameter<T> parameter, String value, String language);

    /**
     * Sets value of the specified parameter in the query.
     * <p>
     * If a value was already specified for the parameter, it is overwritten by the new one. This version inserts the
     * string representation of the value directly into the query, without any type information.
     *
     * @param parameter Parameter object
     * @param value     Value to use
     * @throws IllegalArgumentException If there is no such parameter in the query
     * @see #setParameter(Parameter, Object)
     */
    <T> void setUntypedParameter(Parameter<T> parameter, Object value);

    /**
     * Sets the position of the first result to retrieve.
     *
     * @param startPosition The position to set, starting at 0
     */
    void setFirstResult(int startPosition);

    /**
     * Gets the currently set first result position.
     *
     * @return the first result position, 0 if none was set
     */
    int getFirstResult();

    /**
     * Sets the maximum number of results the query should retrieve.
     *
     * @param maxResults The maximum number of results
     */
    void setMaxResults(int maxResults);

    /**
     * Gets the currently set maximum number of results to retrieve
     *
     * @return Maximum results value, {@code Integer.MAX_VALUE} when none was set
     */
    int getMaxResults();

    /**
     * Clears any previously set value of the specified parameter.
     *
     * @param parameter Parameter object
     * @throws IllegalArgumentException If there is no such parameter in the query
     */
    void clearParameter(Parameter<?> parameter);

    /**
     * Clears any previously set parameter values in this query.
     */
    void clearParameters();

    /**
     * Assembles the query, using any parameter values specified, and returns it as a string.
     *
     * @return Assembled query
     */
    String assembleQuery();

    /**
     * Gets the type of this query.
     *
     * @return Query type
     */
    QueryType getQueryType();

}
