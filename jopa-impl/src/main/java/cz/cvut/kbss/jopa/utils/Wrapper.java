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
