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
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CharacterConverterTest {

    private final CharacterConverter converter = new CharacterConverter();

    @Test
    public void toAttributeSupportsIdentityConversion() {
        assertEquals(Character.valueOf('j'), converter.convertToAttribute("j"));
    }

    @Test
    public void toAttributeThrowsWhenValueIsTooLong() {
        DatatypeMappingException thrown = assertThrows(DatatypeMappingException.class, () -> converter.convertToAttribute("abc"));

        assertEquals(thrown.getMessage(), "Unable to map literal abc to java.lang.Character, because its length is greater than 1");
    }
}
