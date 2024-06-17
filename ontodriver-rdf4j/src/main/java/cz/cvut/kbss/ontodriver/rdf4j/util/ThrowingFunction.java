/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.rdf4j.util;

import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;

import java.util.function.Function;

/**
 * A {@link Function} that may throw a checked exception.
 *
 * @param <T> Argument type
 * @param <R> Result type
 */
public interface ThrowingFunction<T, R> {

    /**
     * Apply the function to the specified argument.
     *
     * @param t Argument
     * @return Result
     * @throws Rdf4jDriverException If function evaluation throws an exception
     */
    R apply(T t) throws Rdf4jDriverException;
}
