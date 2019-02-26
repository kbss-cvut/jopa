/**
 * Copyright (C) 2019 Czech Technical University in Prague
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

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ToDoubleConverterTest {

    private ToDoubleConverter converter = new ToDoubleConverter();

    @Test
    public void toAttributeSupportsWideningIntegerConversion() {
        assertEquals(Double.valueOf(11), converter.convertToAttribute(Byte.valueOf((byte) 11)));
        assertEquals(Double.valueOf(117), converter.convertToAttribute(Short.valueOf((short) 117)));
        assertEquals(Double.valueOf(117), converter.convertToAttribute(117));
        assertEquals(Double.valueOf(117L), converter.convertToAttribute(117L));
        assertEquals(Double.valueOf(3.14), converter.convertToAttribute(3.14F), 0.001);
    }

    @Test
    public void toAttributeSupportsIdentityConversion() {
        assertEquals(Double.valueOf(Math.E), converter.convertToAttribute(Math.E));
    }
}