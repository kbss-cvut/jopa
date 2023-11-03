/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import static org.junit.jupiter.api.Assertions.*;

class XsdDateMapperTest {

    @Test
    void mapParsesSpecifiedStringAndTransformsItToLocalDate() {
        final LocalDate date = LocalDate.now();
        assertEquals(date, XsdDateMapper.map(date.format(DateTimeFormatter.ISO_DATE)));
    }

    @Test
    void mapParsesSpecifiedZonedStringAndTransformsItToLocalDate() {
        final int year = 2000;
        final int month = 12;
        final int day = 20;
        final String value = year + "-" + month + "-" + day + "+02:00";
        assertEquals(LocalDate.of(year, month, day), XsdDateMapper.map(value));
    }

    @Test
    void mapThrowsDatatypeMappingExceptionWhenDateCannotBeParsed() {
        final String invalidValue = "123-321-41";
        assertThrows(DatatypeMappingException.class, () -> XsdDateMapper.map(invalidValue));
    }
}
