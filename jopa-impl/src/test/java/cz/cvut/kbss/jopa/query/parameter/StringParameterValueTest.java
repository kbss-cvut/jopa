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
package cz.cvut.kbss.jopa.query.parameter;

import org.junit.Test;

import static org.junit.Assert.*;

public class StringParameterValueTest {

    @Test
    public void getQueryStringEscapesNewlinesInQueryParameter() {
        final String value = "fdjsakfsa;f\n        fdjsajsfd";
        final String expected = "\"fdjsakfsa;f\\n        fdjsajsfd\"";
        final StringParameterValue sut = new StringParameterValue(value);
        assertEquals(expected, sut.getQueryString());
    }

    @Test
    public void getQueryStringEscapesEscapeSequencesInValue() {
        final String value = "aaa\nbbb\tccc\bdddd\reee\feee\'fff\'ggg\"hhh\"iii\\jjj";
        final String expected = "\"aaa\\nbbb\\tccc\\bdddd\\reee\\feee\\'fff\\'ggg\\\"hhh\\\"iii\\\\jjj\"";

        final StringParameterValue sut = new StringParameterValue(value);
        assertEquals(expected, sut.getQueryString());
    }
}