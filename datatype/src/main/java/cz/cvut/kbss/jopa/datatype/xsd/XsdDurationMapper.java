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

import java.time.Duration;
import java.time.Period;
import java.time.format.DateTimeParseException;

/**
 * Maps {@link cz.cvut.kbss.jopa.vocabulary.XSD#DURATION} values to Java {@link Duration}.
 */
public class XsdDurationMapper {

    private XsdDurationMapper() {
        throw new AssertionError();
    }

    /**
     * Maps the specified value to {@link Duration}.
     *
     * @param value Value to map
     * @return Parsed duration
     */
    public static Object map(String value) {
        try {
            return Duration.parse(value);
        } catch (DateTimeParseException e) {
            try {
                return Period.parse(value);
            } catch (DateTimeParseException e2) {
                throw new DatatypeMappingException("Unable to resolve value of xsd:duration " + value + ". " +
                        "Maybe it is too large for java.time.Duration, but contains time preventing it from being parsed to java.time.Period.", e);
            }
        }
    }
}
