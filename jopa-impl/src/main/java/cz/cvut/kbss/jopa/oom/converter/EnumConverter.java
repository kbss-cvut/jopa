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
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.exception.UnsupportedTypeTransformation;

/**
 * Built-in converter for mapping to/from enum-valued attributes.
 * <p>
 * Uses the default {@link Enum#valueOf(Class, String)} method to transform string-based value loaded from repository to the appropriate
 * attribute type. In the opposite direction, enum value is converted to {@code String}.
 *
 * @param <E>
 */
public class EnumConverter<E extends Enum<E>> implements ConverterWrapper<E, Object> {

    private final Class<E> enumType;

    public EnumConverter(Class<E> enumType) {
        this.enumType = enumType;
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return true;
    }

    @Override
    public Object convertToAxiomValue(E value) {
        return value.toString();
    }

    @Override
    public E convertToAttribute(Object value) {
        try {
            return Enum.valueOf(enumType, value.toString());
        } catch (IllegalArgumentException e) {
            throw new UnsupportedTypeTransformation("Unable to transform value " + value + " to enum " + enumType, e);
        }
    }
}
