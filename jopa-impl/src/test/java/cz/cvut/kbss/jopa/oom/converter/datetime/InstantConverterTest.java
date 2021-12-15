/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
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
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

class InstantConverterTest {

    private final InstantConverter sut = new InstantConverter();

    @Test
    void convertToAxiomValueTransformsInstantToOffsetDateTimeAtUTCZone() {
        final Instant value = Instant.now();
        final Literal result = sut.convertToAxiomValue(value);
        assertEquals(Literal.from(value.toString(), XSD.DATETIME), result);
    }

    @Test
    void convertToAttributeTransformsJavaOffsetDateTimeToInstant() {
        final OffsetDateTime value = OffsetDateTime.now();
        final Instant result = sut.convertToAttribute(value);
        assertEquals(value.toInstant(), result);
    }

    @Test
    void convertToAttributeTransformsLiteralToInstant() {
        final OffsetDateTime value = OffsetDateTime.now();
        final Literal literal = Literal.from(value.toInstant().toString(), XSD.DATETIME);
        final Instant result = sut.convertToAttribute(literal);
        assertEquals(value.toInstant(), result);
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
