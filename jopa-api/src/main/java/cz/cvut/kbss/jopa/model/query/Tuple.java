/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.query;

import java.util.List;

/**
 * Interface for extracting the elements of a query result tuple.
 */
public interface Tuple {

    /**
     * Get the value of the specified tuple element.
     *
     * @param tupleElement
     *            tuple element
     * @return value of tuple element
     * @throws IllegalArgumentException
     *             if tuple element
     *
     *             does not correspond to an element in the
     *
     *             query result tuple
     */
    <X> X get(TupleElement<X> tupleElement);

    /**
     * Get the value of the tuple element to which the specified alias has been
     * assigned.
     *
     * @param alias
     *            alias assigned to tuple element
     * @param type
     *            of the tuple element
     * @return value of the tuple element
     * @throws IllegalArgumentException
     *             if alias
     *
     *             does not correspond to an element in the
     *
     *             query result tuple or element cannot be
     *
     *             assigned to the specified type
     */
    <X> X get(String alias, Class<X> type);

    /**
     * Get the value of the tuple element to which the specified alias has been
     * assigned.
     *
     * @param alias
     *            alias assigned to tuple element
     * @return value of the tuple element
     * @throws IllegalArgumentException
     *             if alias
     *
     *             does not correspond to an element in the
     *
     *             query result tuple
     */
    Object get(String alias);

    /**
     * Get the value of the element at the specified position in the result
     * tuple. The first position is 0.
     *
     * @param i
     *            position in result tuple
     * @param type
     *            type of the tuple element
     * @return value of the tuple element
     * @throws IllegalArgumentException
     *             if i exceeds
     *
     *             length of result tuple or element cannot be
     *
     *             assigned to the specified type
     */
    <X> X get(int i, Class<X> type);

    /**
     * Get the value of the element at the specified position in the result
     * tuple. The first position is 0.
     *
     * @param i
     *            position in result tuple
     * @return value of the tuple element
     * @throws IllegalArgumentException
     *             if i exceeds
     *
     *             length of result tuple
     */
    Object get(int i);

    /**
     * Return the values of the result tuple elements as an array.
     *
     * @return tuple element values
     */
    Object[] toArray();

    /**
     * Return the tuple elements.
     *
     * @return tuple elements
     */
    List<TupleElement<?>> getElements();
}
