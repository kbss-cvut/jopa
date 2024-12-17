/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;
import cz.cvut.kbss.ontodriver.model.LangString;

/**
 * Converts between {@link Character} and a xsd:string representation.
 * <p>
 * This converter supports {@link String} and {@link LangString} as character representations.
 */
public class CharacterConverter implements ConverterWrapper<Character, Object> {

    @Override
    public Object convertToAxiomValue(Character value) {
        assert value != null;
        return value.toString();
    }

    @Override
    public Character convertToAttribute(Object value) {
        assert value != null && value instanceof String;

        if(((String) value).length() > 1) {
            throw new DatatypeMappingException("Unable to map literal " + value + " to " + Character.class.getCanonicalName() + ", because its length is greater than 1");
        }

        return ((String) value).charAt(0);
    }

    @Override
    public boolean supportsAxiomValueType(Class<?> type) {
        return String.class.isAssignableFrom(type);
    }
}
