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

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

import static cz.cvut.kbss.jopa.datatype.DateTimeUtil.SYSTEM_OFFSET;

/**
 * Maps values of {@link cz.cvut.kbss.jopa.vocabulary.XSD#DATETIME} to Java {@link OffsetDateTime}.
 * <p>
 * Note that if the datetime does not specify offset information, system local is used.
 */
public class XsdDateTimeMapper {

    private XsdDateTimeMapper() {
        throw new AssertionError();
    }

    /**
     * Maps the specified value to {@link OffsetDateTime}.
     *
     * @param value Value to map
     * @return Parsed offset datetime
     */
    public static OffsetDateTime map(String value) {
        try {
            return OffsetDateTime.parse(value, DateTimeFormatter.ISO_OFFSET_DATE_TIME);
        } catch (DateTimeParseException e) {
            try {
                LocalDateTime localDateTime = LocalDateTime.parse(value, DateTimeFormatter.ISO_LOCAL_DATE_TIME);
                return OffsetDateTime.of(localDateTime.toLocalDate(), localDateTime.toLocalTime(), SYSTEM_OFFSET);
            } catch (DateTimeParseException e2) {
                throw new DatatypeMappingException("Invalid value provided as xsd:dateTime.", e2);
            }
        }
    }
}
