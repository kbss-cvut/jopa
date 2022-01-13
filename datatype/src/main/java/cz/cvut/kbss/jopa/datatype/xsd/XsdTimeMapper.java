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

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

/**
 * Maps values of {@link cz.cvut.kbss.jopa.vocabulary.XSD#TIME} to Java {@link OffsetTime}.
 * <p>
 * Note that if the time does not specify offset information, system local is used.
 */
public class XsdTimeMapper {

    private static final ZoneOffset LOCAL_OFFSET = ZoneId.systemDefault().getRules().getOffset(LocalDateTime.now());

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
                return LocalTime.parse(value).atOffset(LOCAL_OFFSET);
            } catch (DateTimeParseException e2) {
                throw new DatatypeMappingException("Invalid value provided as xsd:time.", e2);
            }
        }
    }
}
