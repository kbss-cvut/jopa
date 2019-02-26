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

import java.time.Instant;
import java.util.Date;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class InstantConverterTest {

    private final InstantConverter sut = new InstantConverter();

    @Test
    public void convertToAxiomValueTransformsInstantToJavaUtilDate() {
        final Instant value = Instant.now();
        final Date result = sut.convertToAxiomValue(value);
        assertEquals(Date.from(value), result);
    }

    @Test
    public void convertToAttributeTransformsJavaUtilDateToInstant() {
        final Date value = new Date();
        final Instant result = sut.convertToAttribute(value);
        assertEquals(value.toInstant(), result);
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