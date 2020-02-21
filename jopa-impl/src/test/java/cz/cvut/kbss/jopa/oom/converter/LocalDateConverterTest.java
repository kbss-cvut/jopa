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

import org.junit.Test;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public class LocalDateConverterTest {

    private final LocalDateConverter sut = new LocalDateConverter();

    @Test
    public void convertToAxiomValueTransformsLocalDateToJavaUtilDate() {
        final LocalDate value = LocalDate.now();
        final Date result = sut.convertToAxiomValue(value);
        assertNotNull(result);
        assertEquals(java.sql.Date.valueOf(value), result);
    }

    @Test
    public void convertToAttributeTransformsJavaUtilDateToLocalDate() {
        final Date value = new Date();
        final LocalDate result = sut.convertToAttribute(value);
        assertNotNull(result);
        assertEquals(value.toInstant().atZone(ZoneId.systemDefault()).toLocalDate(), result);
    }

    @Test
    public void convertToAxiomReturnsNullForNullInput() {
        assertNull(sut.convertToAxiomValue(null));
    }

    @Test
    public void convertToAttributeReturnsNullForNullInput() {
        assertNull(sut.convertToAttribute(null));
    }
}