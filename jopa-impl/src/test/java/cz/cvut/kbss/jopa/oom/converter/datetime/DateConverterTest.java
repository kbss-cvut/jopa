/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom.converter.datetime;

import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Date;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.*;

class DateConverterTest {

    private final DateConverter sut = new DateConverter();

    @Test
    void convertToAxiomValueConvertsDateToOffsetDateTimeAtUTC() {
        final Date value = new Date();
        final Object result = sut.convertToAxiomValue(value);
        assertThat(result, instanceOf(OffsetDateTime.class));
        assertEquals(value.toInstant().atOffset(ZoneOffset.UTC), result);
    }

    @Test
    void convertToAttributeConvertsOffsetDateTimeToDate() {
        final OffsetDateTime value = OffsetDateTime.now();
        assertEquals(new Date(value.toInstant().toEpochMilli()), sut.convertToAttribute(value));
    }

    @Test
    void convertToAttributeConvertsLiteralToDate() {
        final Date date = new Date();
        final String value = date.toInstant().toString();
        assertEquals(date, sut.convertToAttribute(Literal.from(value, XSD.DATETIME)));
    }

    @Test
    void supportsAxiomValueTypeReturnsTrueForOffsetDateTime() {
        assertTrue(sut.supportsAxiomValueType(OffsetDateTime.class));
        assertFalse(sut.supportsAxiomValueType(Date.class));
    }

    @Test
    void supportsAxiomValueTypeReturnsTrueForLiteral() {
        assertTrue(sut.supportsAxiomValueType(Literal.class));
    }
}
