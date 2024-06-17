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

import java.time.LocalTime;
import java.time.OffsetTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

import static cz.cvut.kbss.jopa.datatype.DateTimeUtil.SYSTEM_OFFSET;

/**
 * Maps values of {@link cz.cvut.kbss.jopa.vocabulary.XSD#TIME} to Java {@link OffsetTime}.
 * <p>
 * Note that if the time does not specify offset information, system local is used.
 */
public class XsdTimeMapper {

    private XsdTimeMapper() {
        throw new AssertionError();
    }

    /**
     * Maps the specified value to {@link OffsetTime}.
     *
     * @param value Value to map
     * @return Parsed offset time
     */
    public static OffsetTime map(String value) {
        try {
            return OffsetTime.parse(value, DateTimeFormatter.ISO_OFFSET_TIME);
        } catch (DateTimeParseException e) {
            try {
                return LocalTime.parse(value).atOffset(SYSTEM_OFFSET);
            } catch (DateTimeParseException e2) {
                throw new DatatypeMappingException("Invalid value provided as xsd:time.", e2);
            }
        }
    }
}
