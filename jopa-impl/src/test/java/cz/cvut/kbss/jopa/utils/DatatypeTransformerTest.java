/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.exception.UnsupportedTypeTransformation;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.net.URL;

import static org.junit.Assert.*;

public class DatatypeTransformerTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void transformReturnsNullForNullInput() {
        assertNull(DatatypeTransformer.transform(null, String.class));
    }

    @Test
    public void transformConvertsValueToTargetType() {
        final Integer value = 117;
        final Double result = DatatypeTransformer.transform(value, Double.class);
        assertEquals(value.doubleValue(), result, 0.01);
    }

    @Test
    public void transformThrowsUnsupportedTypeConversionWhenNoTransformerIsFound() {
        final Integer value = 117;
        thrown.expect(UnsupportedTypeTransformation.class);
        thrown.expectMessage(
                String.format("Cannot transform value %s of type %s to target type %s.", value, Integer.class,
                        URL.class));
        DatatypeTransformer.transform(value, URL.class);
    }

    @Test
    public void transformToStringDoesNotRequireExplicitTransformer() {
        final Integer value = 117;
        final String result = DatatypeTransformer.transform(value, String.class);
        assertEquals(value.toString(), result);
    }

    @Test
    public void transformCastsValueWithoutTransformerWhenValueIsAssignableToTargetType() {
        final Integer value = 117;
        final Number result = DatatypeTransformer.transform(value, Number.class);
        assertSame(result, value);
    }

    @Test
    public void transformSupportsConversionUsingConstructorWithParameterMatchingTransformedValueType() {
        final String value = "http://onto.fel.cvut.cz";
        final URL result = DatatypeTransformer.transform(value, URL.class);
        assertEquals(value, result.toString());
    }

    @Test
    public void transformThrowsUnsupportedTypeConversionWhenTargetTypeDoesNotHaveMatchingConstructor() {
        final String value = "http://onto.fel.cvut.cz";
        thrown.expect(UnsupportedTypeTransformation.class);
        thrown.expectMessage(
                String.format("Cannot transform value %s of type %s to target type %s.", value, String.class,
                        OWLClassA.class));
        DatatypeTransformer.transform(value, OWLClassA.class);
    }
}