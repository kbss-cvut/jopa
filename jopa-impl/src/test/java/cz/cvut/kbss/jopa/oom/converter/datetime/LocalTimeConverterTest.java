/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom.converter.datetime;

import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.zone.ZoneRules;
import java.util.Date;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.*;

class LocalTimeConverterTest {

    private final LocalTimeConverter sut = new LocalTimeConverter();

    @Test
    void convertToAxiomValueTransformsLocalTimeToOffsetTimeAtSystemOffset() {
        final LocalTime value = LocalTime.now();
        final Object result = sut.convertToAxiomValue(value);
        assertThat(result, instanceOf(OffsetTime.class));
        final ZoneRules zoneRules = ZoneId.systemDefault().getRules();
        assertEquals(value.atOffset(zoneRules.getOffset(LocalDateTime.now())), result);
    }

    @Test
    void convertToAttributeTransformsJavaOffsetTimeToLocalDateTime() {
        final OffsetTime value = OffsetTime.now();
        final LocalTime result = sut.convertToAttribute(value);
        assertEquals(value.toLocalTime(), result);
    }

    @Test
    void convertToAttributeTransformsLiteralToLocalTime() {
        final OffsetTime value = OffsetTime.now();
        final Literal literal = Literal.from(value.format(DateTimeFormatter.ISO_OFFSET_TIME), XSD.TIME);
        final LocalTime result = sut.convertToAttribute(literal);
        assertEquals(value.toLocalTime(), result);
    }

    @Test
    void supportsAxiomValueTypeReturnsTrueForOffsetTime() {
        assertTrue(sut.supportsAxiomValueType(OffsetTime.class));
        assertFalse(sut.supportsAxiomValueType(Date.class));
    }

    @Test
    void supportsAxiomValueTypeReturnsTrueForLiteral() {
        assertTrue(sut.supportsAxiomValueType(Literal.class));
    }
}
