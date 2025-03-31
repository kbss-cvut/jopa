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
package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.vocabulary.XSD;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.zone.ZoneRules;

import static org.junit.jupiter.api.Assertions.assertEquals;

class TemporalParameterValueTest {

    private static ZoneOffset systemOffset;

    @BeforeAll
    static void setUpBeforeAll() {
        final ZoneRules zoneRules = ZoneId.systemDefault().getRules();
        systemOffset = zoneRules.getOffset(LocalDateTime.now());
    }

    @Test
    void getQueryStringConvertsProvidedLocalDateTimeValueToXsdDateTimeStringAtLocalSystemOffset() {
        final LocalDateTime value = LocalDateTime.now();
        final String result = new TemporalParameterValue(value).getQueryString();
        assertEquals(generateExpectedString(value.atOffset(systemOffset).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), XSD.DATETIME), result);
    }

    static String generateExpectedString(String value, String datatype) {
        return "\"" + value + "\"^^<" + datatype + ">";
    }

    @Test
    void getQueryStringConvertsProvidedLocalTimeValueToXsdTimeStringAtLocalSystemOffset() {
        final LocalTime value = LocalTime.now();
        final String result = new TemporalParameterValue(value).getQueryString();
        assertEquals(generateExpectedString(value.atOffset(systemOffset).format(DateTimeFormatter.ISO_OFFSET_TIME), XSD.TIME), result);
    }

    @Test
    void getQueryStringConvertsProvidedLocalDateToXsdDateString() {
        final LocalDate value = LocalDate.now();
        final String result = new TemporalParameterValue(value).getQueryString();
        assertEquals(generateExpectedString(value.format(DateTimeFormatter.ISO_DATE), XSD.DATE), result);
    }

    @Test
    void getQueryStringConvertsProvidedInstantToXsdDateTimeString() {
        final Instant value = Instant.now();
        final String result = new TemporalParameterValue(value).getQueryString();
        assertEquals(generateExpectedString(value.atOffset(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), XSD.DATETIME), result);
    }
}
