/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
