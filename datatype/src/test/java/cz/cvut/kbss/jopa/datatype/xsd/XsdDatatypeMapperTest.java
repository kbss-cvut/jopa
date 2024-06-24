/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class XsdDatatypeMapperTest {

    private final XsdDatatypeMapper sut = XsdDatatypeMapper.getInstance();

    @ParameterizedTest
    @MethodSource("mapTestArguments")
    void mapCorrectlyConvertsValues(Object expected, Literal literal) {
        final Optional<Object> result = sut.map(literal);
        assertTrue(result.isPresent());
        assertEquals(expected, result.get());
    }

    static Stream<Arguments> mapTestArguments() {
        final long longValue = System.currentTimeMillis();
        final LocalDate date = LocalDate.now();
        final OffsetTime time = OffsetTime.of(12, 45, 15, 0, ZoneOffset.UTC);
        final OffsetDateTime dateTime = OffsetDateTime.of(2021, 11, 17, 12, 23, 10, 0, ZoneOffset.UTC);
        final Duration duration = Duration.ofHours(24).plusMinutes(15).plusSeconds(44);
        return Stream.of(
                Arguments.of(true, Literal.from(Boolean.toString(true), XSD.BOOLEAN)),
                Arguments.of((byte) 101, Literal.from(Byte.toString((byte) 101), XSD.BYTE)),
                Arguments.of((short) 115, Literal.from(Byte.toString((byte) 115), XSD.UNSIGNED_BYTE)),
                Arguments.of(1234, Literal.from(Short.toString((short) 1234), XSD.UNSIGNED_SHORT)),
                Arguments.of((long) Integer.MAX_VALUE, Literal.from(Integer.toString(Integer.MAX_VALUE), XSD.UNSIGNED_INT)),
                Arguments.of(BigInteger.valueOf(Integer.MIN_VALUE), Literal.from(Integer.toString(Integer.MIN_VALUE), XSD.INTEGER)),
                Arguments.of(BigInteger.valueOf(longValue), Literal.from(Long.toString(longValue), XSD.UNSIGNED_LONG)),
                Arguments.of("test", Literal.from("test", XSD.STRING)),
                Arguments.of("test", Literal.from("test", XSD.NORMALIZED_STRING)),
                Arguments.of(date,Literal.from(date.format(DateTimeFormatter.ISO_DATE), XSD.DATE) ),
                Arguments.of(time, Literal.from(time.format(DateTimeFormatter.ISO_TIME), XSD.TIME)),
                Arguments.of(dateTime, Literal.from(dateTime.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), XSD.DATETIME)),
                Arguments.of(duration, Literal.from(duration.toString(), XSD.DURATION)),
                Arguments.of(URI.create("https://www.w3.org/TR/xmlschema-2/#anyURI"), Literal.from("https://www.w3.org/TR/xmlschema-2/#anyURI", XSD.ANY_URI)),
                Arguments.of(new BigDecimal("3.141952"), Literal.from("3.141952", XSD.DECIMAL)),
                Arguments.of(Float.NaN, Literal.from("NaN", XSD.FLOAT)),
                Arguments.of(Float.NEGATIVE_INFINITY, Literal.from("-INF", XSD.FLOAT)),
                Arguments.of(Float.POSITIVE_INFINITY, Literal.from("INF", XSD.FLOAT)),
                Arguments.of(Double.NaN, Literal.from("NaN", XSD.DOUBLE)),
                Arguments.of(Double.NEGATIVE_INFINITY, Literal.from("-INF", XSD.DOUBLE)),
                Arguments.of(Double.POSITIVE_INFINITY, Literal.from("INF", XSD.DOUBLE))
        );
    }

    @Test
    void mapReturnsEmptyOptionalForUnknownDatatype() {
        final String datatype = XSD.NAMESPACE + "NOTATION";
        assertFalse(sut.map(Literal.from("jpeg", datatype)).isPresent());
    }

    @Test
    void mapThrowsDatatypeMappingExceptionForInvalidLiteralValue() {
        final Literal invalidValue = Literal.from("abcd", XSD.INT);
        assertThrows(DatatypeMappingException.class, () -> sut.map(invalidValue));
    }
}
