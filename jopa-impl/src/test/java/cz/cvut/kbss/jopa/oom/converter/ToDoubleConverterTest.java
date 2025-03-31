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

class ToDoubleConverterTest {

    private final ToDoubleConverter converter = new ToDoubleConverter();

    @Test
    void toAttributeSupportsWideningIntegerConversion() {
        assertEquals(Double.valueOf(11), converter.convertToAttribute((byte) 11));
        assertEquals(Double.valueOf(117), converter.convertToAttribute((short) 117));
        assertEquals(Double.valueOf(117), converter.convertToAttribute(117));
        assertEquals(Double.valueOf(117L), converter.convertToAttribute(117L));
        assertEquals(3.14, converter.convertToAttribute(3.14F), 0.001);
    }

    @Test
    void toAttributeSupportsIdentityConversion() {
        assertEquals(Double.valueOf(Math.E), converter.convertToAttribute(Math.E));
    }
}
