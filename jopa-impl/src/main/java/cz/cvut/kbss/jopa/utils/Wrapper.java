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
package cz.cvut.kbss.jopa.utils;

/**
 * Marks classes which allow to unwrap provider-specific implementations.
 */
@FunctionalInterface
public interface Wrapper {

    /**
     * Unwraps implementation of the specified class.
     *
     * @param cls The class of the object to be returned
     * @return An instance of the specified class
     * @throws cz.cvut.kbss.jopa.exceptions.OWLPersistenceException If the provider does not support the class
     */
    <T> T unwrap(Class<T> cls);
}
