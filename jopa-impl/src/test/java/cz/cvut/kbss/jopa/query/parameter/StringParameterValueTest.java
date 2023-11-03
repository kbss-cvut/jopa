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
package cz.cvut.kbss.jopa.query.parameter;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

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
        final String value = "aaa\nbbb\tccc\bdddd\reee\feee'fff'ggg\"hhh\"iii\\jjj";
        final String expected = "\"aaa\\nbbb\\tccc\\bdddd\\reee\\feee\\'fff\\'ggg\\\"hhh\\\"iii\\\\jjj\"";

        final StringParameterValue sut = new StringParameterValue(value);
        assertEquals(expected, sut.getQueryString());
    }
}
