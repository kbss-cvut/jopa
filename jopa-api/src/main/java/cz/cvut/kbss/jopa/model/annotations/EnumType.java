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
package cz.cvut.kbss.jopa.model.annotations;

/**
 * Defines mapping for enumerated types.
 * <p>
 * The constants of this enumerated type specify how a persistent property or field of an enumerated type should be
 * persisted.
 */
public enum EnumType {
    /**
     * Persist enumerated type property or field as an individual assumed to be an element of a {@literal
     * owl:ObjectOneOf} enumeration.
     * <p>
     * Note that in this case the enum constants must be annotated with {@link Individual} mapping them to ontological
     * individuals.
     */
    OBJECT_ONE_OF,

    /**
     * Persist enumerated type property or field as an integer representing the ordinal number of the enumerated
     * constant.
     */
    ORDINAL,

    /**
     * Persist enumerated type property or field as a string representation of the enumerated constant.
     */
    STRING
}
