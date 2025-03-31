/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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

class ToShortConverterTest {

    private final ToShortConverter converter = new ToShortConverter();

    @Test
    void toAttributeSupportsWideningIntegerConversion() {
        assertEquals(Short.valueOf((short) 11), converter.convertToAttribute((byte) 11));
    }

    @Test
    void toAttributeSupportsIdentityConversion() {
        assertEquals(Short.valueOf((short) 11), converter.convertToAttribute((short) 11));
    }
}
