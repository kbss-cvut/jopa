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

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ToIntegerConverterTest {

    private final ToIntegerConverter converter = new ToIntegerConverter();

    @Test
    void toAttributeSupportsWideningIntegerConversion() {
        assertEquals(Integer.valueOf(11), converter.convertToAttribute((byte) 11));
        assertEquals(Integer.valueOf(117), converter.convertToAttribute((short) 117));
    }

    @Test
    void toAttributeSupportsIdentityConversion() {
        assertEquals(Integer.valueOf(117), converter.convertToAttribute(117));
    }
}
