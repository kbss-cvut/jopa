/**
 * Copyright (C) 2023 Czech Technical University in Prague
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

import java.time.Duration;
import java.time.Period;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class XsdDurationMapperTest {

    @Test
    void mapParsesTimeBasedDuration() {
        final Duration duration = Duration.ofHours(5).plusMinutes(10).plusSeconds(15);
        assertEquals(duration, XsdDurationMapper.map(duration.toString()));
    }

    @Test
    void mapParsesDurationWithDaysToDuration() {
        final String value = "P2DT5H10M15S";
        final Duration expected = Duration.ofHours(53).plusMinutes(10).plusSeconds(15);
        assertEquals(expected, XsdDurationMapper.map(value));
    }

    @Test
    void mapParsesDurationWithLargeNumberOfYearsToPeriod() {
        final String value = "P" + Integer.MAX_VALUE + "Y";
        assertEquals(Period.ofYears(Integer.MAX_VALUE), XsdDurationMapper.map(value));
    }

    @Test
    void mapThrowsDatatypeMappingExceptionForDurationWithLargeNumberOfYearsAndTime() {
        // The value is too large to fit in Duration, but parsing as Period would lose the time, so Period will fail as well
        final String value = "P" + Integer.MAX_VALUE + "YT10H15M8S";
        assertThrows(DatatypeMappingException.class, () -> XsdDurationMapper.map(value));
    }

    @Test
    void mapThrowsDatatypeMappingExceptionForInvalidDurationValue() {
        final String invalidValue = "P343M123DT23:15";
        assertThrows(DatatypeMappingException.class, () -> XsdDurationMapper.map(invalidValue));
    }
}
