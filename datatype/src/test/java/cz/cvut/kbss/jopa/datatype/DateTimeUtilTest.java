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
package cz.cvut.kbss.jopa.datatype;

import org.junit.jupiter.api.Test;

import java.time.*;

import static org.junit.jupiter.api.Assertions.assertEquals;

class DateTimeUtilTest {

    @Test
    void localDateTimeToDateTimeUsesSystemOffset() {
        final LocalDateTime value = LocalDateTime.now();
        ZoneOffset offset = ZoneId.systemDefault().getRules().getOffset(value);
        final OffsetDateTime expected = OffsetDateTime.of(value.toLocalDate(), value.toLocalTime(), offset);
        assertEquals(expected, DateTimeUtil.toDateTime(value));
    }

    @Test
    void instantToDateTimeAssumesUTCZone() {
        final Instant value = Instant.now();
        final OffsetDateTime expected = value.atOffset(ZoneOffset.UTC);
        assertEquals(expected, DateTimeUtil.toDateTime(value));
    }

    @Test
    void localTimeToTimeUsesSystemOffset() {
        final LocalTime value = LocalTime.now();
        ZoneOffset offset = ZoneId.systemDefault().getRules().getOffset(LocalDateTime.now());
        final OffsetTime expected = OffsetTime.of(value, offset);
        assertEquals(expected, DateTimeUtil.toTime(value));
    }
}
