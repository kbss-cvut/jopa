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
package cz.cvut.kbss.jopa.model.query;

/**
 * Type for query parameter objects.
 *
 * @param <T> the type of the parameter
 */
public interface Parameter<T> {

    /**
     * Return the parameter name, or null if the parameter is not a named parameter or no name has been assigned.
     *
     * @return parameter name
     */
    String getName();

    /**
     * Return the parameter position, or null if the parameter is not a positional parameter.
     *
     * @return position of parameter
     */
    Integer getPosition();

    /**
     * Return the Java type of the parameter. Values bound to the parameter must be assignable to this type. This method
     * is required to be supported for criteria queries only. Applications that use this method for Java Persistence
     * query language queries and native queries will not be portable.
     *
     * @return the Java type of the parameter
     * @throws IllegalStateException if invoked on a parameter obtained from a Java persistence query language query or
     *                               native query when the implementation does not support this use
     */
    Class<T> getParameterType();
}
