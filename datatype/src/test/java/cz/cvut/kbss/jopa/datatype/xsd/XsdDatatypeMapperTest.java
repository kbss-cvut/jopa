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
package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;

import java.math.BigInteger;
import java.net.URI;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

class XsdDatatypeMapperTest {

    private final XsdDatatypeMapper sut = XsdDatatypeMapper.getInstance();

    @Test
    void mapReturnsUnsignedByteAsShort() {
        final byte value = 115;
        final Literal literal = Literal.from(Byte.toString(value), XSD.UNSIGNED_BYTE);
        assertEquals((short) value, map(literal));
    }

    private Object map(Literal literal) {
        final Optional<Object> result = sut.map(literal);
        assertTrue(result.isPresent());
        return result.get();
    }

    @Test
    void mapReturnsEmptyOptionalForUnknownDatatype() {
        final String datatype = XSD.NAMESPACE + "NOTATION";
        assertFalse(sut.map(Literal.from("jpeg", datatype)).isPresent());
    }

    @Test
    void mapReturnsUnsignedShortAsInt() {
        final short value = 1234;
        final Literal literal = Literal.from(Short.toString(value), XSD.UNSIGNED_SHORT);
        assertEquals((int) value, map(literal));
    }

    @Test
    void mapThrowsDatatypeMappingExceptionForInvalidLiteralValue() {
        final Literal invalidValue = Literal.from("abcd", XSD.INT);
        assertThrows(DatatypeMappingException.class, () -> sut.map(invalidValue));
    }

    @Test
    void mapReturnsUnsignedIntAsLong() {
        final int value = Integer.MAX_VALUE;
        final Literal literal = Literal.from(Integer.toString(value), XSD.UNSIGNED_INT);
        assertEquals((long) value, map(literal));
    }

    @Test
    void mapReturnsIntegerAsBigInteger() {
        final int value = Integer.MIN_VALUE;
        final Literal literal = Literal.from(Integer.toString(value), XSD.INTEGER);
        assertEquals(BigInteger.valueOf(value), map(literal));
    }

    @Test
    void mapReturnsUnsignedLongAsBigInteger() {
        final long value = System.currentTimeMillis();
        final Literal literal = Literal.from(Long.toString(value), XSD.UNSIGNED_LONG);
        assertEquals(BigInteger.valueOf(value), map(literal));
    }

    @Test
    void mapReturnsXsdStringAsString() {
        final String value = "test";
        final Literal literal = Literal.from(value, XSD.STRING);
        assertEquals(value, map(literal));
    }

    @Test
    void mapReturnsXsdNormalizedStringAsString() {
        final String value = "test";
        final Literal literal = Literal.from(value, XSD.NORMALIZED_STRING);
        assertEquals(value, map(literal));
    }

    @Test
    void mapReturnsLocalDateForXsdDate() {
        final LocalDate date = LocalDate.now();
        final Literal literal = Literal.from(date.format(DateTimeFormatter.ISO_DATE), XSD.DATE);
        assertEquals(date, map(literal));
    }

    @Test
    void mapReturnsOffsetTimeForXsdTime() {
        final OffsetTime time = OffsetTime.of(12, 45, 15, 0, ZoneOffset.UTC);
        assertEquals(time, map(Literal.from(time.format(DateTimeFormatter.ISO_TIME), XSD.TIME)));
    }

    @Test
    void mapReturnsOffsetDateTimeForXsdDateTime() {
        final OffsetDateTime dateTime = OffsetDateTime.of(2021, 11, 17, 12, 23, 10, 0, ZoneOffset.UTC);
        assertEquals(dateTime, map(Literal.from(dateTime.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), XSD.DATETIME)));
    }

    @Test
    void mapReturnsDurationForXsdDuration() {
        final Duration duration = Duration.ofHours(24).plusMinutes(15).plusSeconds(44);
        assertEquals(duration, map(Literal.from(duration.toString(), XSD.DURATION)));
    }

    @Test
    void mapReturnsUriForXsdAnyUri() {
        final String uri = "https://www.w3.org/TR/xmlschema-2/#anyURI";
        assertEquals(URI.create(uri), map(Literal.from(uri, XSD.ANY_URI)));
    }
}
