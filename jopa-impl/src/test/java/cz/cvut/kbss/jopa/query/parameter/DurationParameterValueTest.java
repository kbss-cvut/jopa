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
package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.time.Period;

import static cz.cvut.kbss.jopa.query.parameter.TemporalParameterValueTest.generateExpectedString;
import static org.junit.jupiter.api.Assertions.assertEquals;

class DurationParameterValueTest {

    @Test
    void getQueryStringReturnsXsdDurationForJavaDurationValue() {
        final Duration value = Duration.ofHours(Generators.randomPositiveInt(24));
        assertEquals(generateExpectedString(value.toString(), XSD.DURATION), new DurationParameterValue(value).getQueryString());
    }

    @Test
    void getQueryStringReturnsXsdDurationForJavaPeriodValue() {
        final Period value = Period.of(2, 8, Generators.randomPositiveInt(30));
        assertEquals(generateExpectedString(value.toString(), XSD.DURATION), new DurationParameterValue(value).getQueryString());
    }
}
