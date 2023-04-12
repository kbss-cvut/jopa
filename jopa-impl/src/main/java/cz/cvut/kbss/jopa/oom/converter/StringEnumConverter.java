/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.datatype.exception.UnsupportedTypeTransformationException;

/**
 * Built-in converter for mapping to/from enum-valued attributes.
 * <p>
 * Uses the default {@link Enum#valueOf(Class, String)} method to transform string-based value loaded from repository to
 * the appropriate attribute type. In the opposite direction, enum value is converted to {@code String}. It is used for
 * enumerated attributes with {@link cz.cvut.kbss.jopa.model.annotations.EnumType#ORDINAL} configuration.
 *
 * @param <E> Enum type
 */
public class StringEnumConverter<E extends Enum<E>> implements ConverterWrapper<E, Object> {

    private final Class<E> enumType;

    public StringEnumConverter(Class<E> enumType) {
        this.enumType = enumType;
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return true;
    }

    @Override
    public Object convertToAxiomValue(E value) {
        return value != null ? value.toString() : null;
    }

    @Override
    public E convertToAttribute(Object value) {
        try {
            return Enum.valueOf(enumType, ToLexicalFormConverter.INSTANCE.convertToAttribute(value));
        } catch (IllegalArgumentException e) {
            throw new UnsupportedTypeTransformationException(
                    "Unable to transform value " + value + " to enum " + enumType, e);
        }
    }
}
