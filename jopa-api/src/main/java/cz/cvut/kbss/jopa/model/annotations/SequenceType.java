/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
 * Defines types of sequence modeling in OWL.
 */
public enum SequenceType {
    /**
     * Used for simple (nonreferenced) sequences.
     *
     * This means that elements of the sequence are unique to the sequence owner
     * and are NOT shared with other sequences.
     *
     * TODO example
     */
    simple,

    /**
     * Used for referenced sequences. This case is more general, but sequence
     * representation requires more space (linear in the original size)
     *
     * This means that elements of the sequence are not unique to the sequence
     * owner. Thus these elements might be referenced by other sequences.
     *
     * TODO example
     */
    referenced
}
