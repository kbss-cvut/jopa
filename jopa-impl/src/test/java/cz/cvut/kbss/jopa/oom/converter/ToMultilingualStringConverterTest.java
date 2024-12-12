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
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Translations;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.net.URI;
import java.util.Map;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ToMultilingualStringConverterTest {

    private final ToMultilingualStringConverter sut = new ToMultilingualStringConverter();

    @ParameterizedTest
    @MethodSource("supportedTypes")
    void supportsReturnsTrueForSupportedAxiomValueTypes(Class<?> type) {
        assertTrue(sut.supportsAxiomValueType(type));
    }

    static Stream<Arguments> supportedTypes() {
        return Stream.of(
                Arguments.of(Translations.class),
                Arguments.of(LangString.class),
                Arguments.of(String.class)
        );
    }

    @Test
    void supportsReturnsFalseForUnsupportedAxiomValueType() {
        assertFalse(sut.supportsAxiomValueType(URI.class));
    }

    @Test
    void convertToAxiomValueReturnsOntoDriverMultilingualStringWithProvidedValue() {
        final cz.cvut.kbss.jopa.model.MultilingualString value = cz.cvut.kbss.jopa.model.MultilingualString.create("en", "Test");

        final Object result = sut.convertToAxiomValue(value);
        assertInstanceOf(Translations.class, result);
        assertEquals(value.getValue(), ((Translations) result).getValue());
    }

    @Test
    void convertToAttributeConvertsOntoDriverMultilingualStringToJopaMultilingualString() {
        final Translations value = new Translations(Map.of("en", "Test"));

        final cz.cvut.kbss.jopa.model.MultilingualString result = sut.convertToAttribute(value);
        assertEquals(value.getValue(), result.getValue());
    }

    @Test
    void convertToAttributeConvertsLangStringToJopaMultilingualString() {
        final LangString value = new LangString("Test", "cs");

        final cz.cvut.kbss.jopa.model.MultilingualString result = sut.convertToAttribute(value);
        assertEquals(cz.cvut.kbss.jopa.model.MultilingualString.create(value.getValue(), value.getLanguage()
                                                                                              .get()), result);
    }

    @Test
    void convertToAttributeConvertsPlainStringToJopaMultilingualString() {
        final cz.cvut.kbss.jopa.model.MultilingualString result = sut.convertToAttribute("Test");
        assertEquals(cz.cvut.kbss.jopa.model.MultilingualString.create("Test", null), result);
    }
}
