/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.annotations;

/**
 * Defines strategies for fetching data from the database.
 * <p>
 * The {@code EAGER} strategy is a requirement on the persistence provider runtime that data must be eagerly fetched.
 * The {@code LAZY} strategy is a hint to the persistence provider runtime that data should be fetched lazily when it is
 * first accessed. The implementation is permitted to eagerly fetch data for which the {@code LAZY} strategy hint has
 * been specified.
 */
public enum FetchType {
    /**
     * Defines that data can be lazily fetched.
     */
    LAZY,
    /**
     * Defines that data must be eagerly fetched.
     */
    EAGER
}
