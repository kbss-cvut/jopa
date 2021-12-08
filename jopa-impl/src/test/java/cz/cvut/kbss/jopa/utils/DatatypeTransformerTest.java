/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.exception.UnsupportedTypeTransformationException;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.Test;

import java.net.URL;

import static org.junit.jupiter.api.Assertions.*;

class DatatypeTransformerTest {

    @Test
    void transformReturnsNullForNullInput() {
        assertNull(DatatypeTransformer.transform(null, String.class));
    }

    @Test
    void transformConvertsValueToTargetType() {
        final Integer value = 117;
        final Double result = DatatypeTransformer.transform(value, Double.class);
        assertEquals(value.doubleValue(), result, 0.01);
    }

    @Test
    void transformThrowsUnsupportedTypeConversionWhenNoTransformerIsFound() {
        final Integer value = 117;
        final UnsupportedTypeTransformationException ex = assertThrows(UnsupportedTypeTransformationException.class,
                () -> DatatypeTransformer.transform(value, URL.class));
        assertEquals(String.format("Cannot transform value %s of type %s to target type %s.", value, Integer.class,
                URL.class), ex.getMessage());
    }

    @Test
    void transformToStringDoesNotRequireExplicitTransformer() {
        final Integer value = 117;
        final String result = DatatypeTransformer.transform(value, String.class);
        assertEquals(value.toString(), result);
    }

    @Test
    void transformCastsValueWithoutTransformerWhenValueIsAssignableToTargetType() {
        final Integer value = 117;
        final Number result = DatatypeTransformer.transform(value, Number.class);
        assertSame(result, value);
    }

    @Test
    void transformSupportsConversionUsingConstructorWithParameterMatchingTransformedValueType() {
        final String value = "http://onto.fel.cvut.cz";
        final URL result = DatatypeTransformer.transform(value, URL.class);
        assertEquals(value, result.toString());
    }

    @Test
    void transformThrowsUnsupportedTypeConversionWhenTargetTypeDoesNotHaveMatchingConstructor() {
        final String value = "http://onto.fel.cvut.cz";
        final UnsupportedTypeTransformationException ex = assertThrows(UnsupportedTypeTransformationException.class,
                () -> DatatypeTransformer.transform(value, OWLClassA.class));
        assertEquals(String.format("Cannot transform value %s of type %s to target type %s.", value, String.class,
                OWLClassA.class), ex.getMessage());
    }

    @Test
    void transformTransformsLangStringToString() {
        final LangString value = new LangString("test", "en");
        final String result = DatatypeTransformer.transform(value, String.class);
        assertEquals(value.getValue(), result);
    }
}
