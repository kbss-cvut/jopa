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
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class XsdDateTimeMapperTest {

    @Test
    void mapParsesDateTimeWithLocalOffset() {
        final OffsetDateTime dateTime = OffsetDateTime.now();
        assertEquals(dateTime, XsdDateTimeMapper.map(dateTime.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)));
    }

    @Test
    void mapParsesDateTimeWithUtcOffset() {
        final OffsetDateTime dateTime = OffsetDateTime.of(LocalDateTime.now(), ZoneOffset.UTC);
        assertEquals(dateTime, XsdDateTimeMapper.map(dateTime.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)));
    }

    @Test
    void mapParsesDateTimeWithoutOffsetUsingLocalOffset() {
        final LocalDateTime dateTime = LocalDateTime.now();
        assertEquals(
                OffsetDateTime.of(dateTime, ZoneId.systemDefault().getRules().getOffset(LocalDateTime.now())),
                XsdDateTimeMapper.map(dateTime.format(DateTimeFormatter.ISO_DATE_TIME)));
    }

    @Test
    void mapThrowsDatatypeMappingExceptionWhenValueCannotBeParsed() {
        final String invalidValue = "2021-12-45T25:16:22Zzz";
        assertThrows(DatatypeMappingException.class, () -> XsdDateTimeMapper.map(invalidValue));
    }
}
