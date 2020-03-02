/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

import org.junit.jupiter.api.Test;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class ZonedDateTimeConverterTest {

    private final ZonedDateTimeConverter sut = new ZonedDateTimeConverter();

    @Test
    void convertToAxiomValueTransformsZonedDateTimeToJavaUtilDate() {
        final ZonedDateTime value = ZonedDateTime.now();
        final Date result = sut.convertToAxiomValue(value);
        assertEquals(Date.from(value.toInstant()), result);
    }

    @Test
    void convertToAttributeTransformsJavaUtilDateToZonedDateTime() {
        final Date value = new Date();
        final ZonedDateTime result = sut.convertToAttribute(value);
        assertEquals(value.toInstant().atZone(ZoneId.systemDefault()), result);
    }

    @Test
    void convertToAxiomReturnsNullForNullInput() {
        assertNull(sut.convertToAxiomValue(null));
    }

    @Test
    void convertToAttributeReturnsNullForNullInput() {
        assertNull(sut.convertToAttribute(null));
    }
}
