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
package cz.cvut.kbss.jopa.utils;

/**
 * Represents an operation that accepts a single input argument and returns no result. Unlike most other functional
 * interfaces, {@code ThrowingConsumer} is expected to operate via side-effects.
 * <p>
 * This is a functional interface whose functional method is {@link #accept(Object)}.
 * <p>
 * This version allows the accept method to throw checked exceptions.
 *
 * @param <T> the type of the input to the operation
 * @param <E> The type of the checked exception thrown by the consumer code
 */
@FunctionalInterface
public interface ThrowingConsumer<T, E extends Exception> {

    void accept(T t) throws E;
}
