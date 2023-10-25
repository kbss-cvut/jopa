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
package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.datatype.exception.DatatypeMappingException;
import org.junit.jupiter.api.Test;

import java.time.*;
import java.time.format.DateTimeFormatter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class XsdTimeMapperTest {

    @Test
    void mapParsesTimeWithoutOffsetToOffsetTimeWithLocalOffset() {
        final int hour = 12;
        final int minute = 11;
        final int second = 10;
        final String literal = hour + ":" + minute + ":" + second;
        assertEquals(
                LocalTime.of(hour, minute, second).atOffset(ZoneId.systemDefault().getRules().getOffset(LocalDateTime.now())),
                XsdTimeMapper.map(literal));
    }

    @Test
    void mapParsesTimeWithZeroOffset() {
        final OffsetTime time = OffsetTime.now().withOffsetSameLocal(ZoneOffset.UTC);
        assertEquals(time, XsdTimeMapper.map(time.format(DateTimeFormatter.ISO_TIME)));
    }

    @Test
    void mapParsesTimeWithExplicitOffset() {
        final OffsetTime time = OffsetTime.now();
        assertEquals(time, XsdTimeMapper.map(time.format(DateTimeFormatter.ISO_TIME)));
    }

    @Test
    void mapThrowsDatatypeMappingExceptionWhenTimeCannotBeParsed() {
        final String invalidValue = "12:78:da";
        assertThrows(DatatypeMappingException.class, () -> XsdTimeMapper.map(invalidValue));
    }
}
