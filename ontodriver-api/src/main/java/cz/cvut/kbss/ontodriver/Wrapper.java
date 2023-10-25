/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;

/**
 * Interface for retrieving concrete implementations of the OntoDriver API and/or classes the implementations use (e.g.
 * classes of the underlying storage framework).
 */
@FunctionalInterface
public interface Wrapper {

    /**
     * Returns an object that implements the given interface to allow access to non-standard methods, or standard
     * methods not exposed by the proxy. If the receiver implements the interface then the result is the receiver or a
     * proxy for the receiver. If the receiver is a wrapper and the wrapped object implements the interface then the
     * result is the wrapped object or a proxy for the wrapped object. Otherwise return the the result of calling unwrap
     * recursively on the wrapped object or a proxy for that result. If the receiver is not a wrapper and does not
     * implement the interface, then an {@link OntoDriverException} is thrown.
     *
     * @param cls The type of the required result
     * @param <T> The type of the class modelled by this Class object
     * @return An object implementing the interface
     * @throws OntoDriverException If no matching object is found
     */
    <T> T unwrap(Class<T> cls) throws OntoDriverException;
}
