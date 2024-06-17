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

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ToLangStringConverterTest {

    private final ToLangStringConverter sut = new ToLangStringConverter();

    @Test
    void convertToAttributeReturnsValueWhenItIsLangStringAlready() {
        final LangString value = new LangString("test value " + Generators.randomInt(), "en");
        assertSame(value, sut.convertToAttribute(value));
    }

    @Test
    void convertToAttributeCreatesLangStringWithoutLanguageWhenStringIsProvided() {
        final String value = "test value" + Generators.randomInt();
        assertEquals(new LangString(value), sut.convertToAttribute(value));
    }
}