/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.ontodriver.model.LangString;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MultilingualStringCondenserTest {

    private final MultilingualStringCondenser sut = new MultilingualStringCondenser();

    @Test
    void getValuesReturnsEmptyListWhenNoValueWasAdded() {
        assertTrue(sut.getValues().isEmpty());
    }

    @Test
    void getValuesReturnsUnmodifiableList() {
        sut.add(new LangString("test", "en"));
        assertThrows(UnsupportedOperationException.class, () -> sut.getValues().add(new MultilingualString()));
    }

    @Test
    void addCreatesNewMultilingualStringForValueWithLanguage() {
        sut.add(new LangString("test", "en"));
        assertEquals(List.of(MultilingualString.create("test", "en")), sut.getValues());
    }

    @Test
    void addMergesValueIntoExistingMultilingualStringLackingItsLanguage() {
        sut.add(new LangString("test", "en"));
        sut.add(new LangString("test", "cs"));

        assertEquals(1, sut.getValues().size());
        final MultilingualString expected = MultilingualString.create("test", "en");
        expected.set("cs", "test");
        assertEquals(expected, sut.getValues().get(0));
    }

    @Test
    void addCreatesNewMultilingualStringForValueWithLanguageAlreadyPresentInExistingOne() {
        sut.add(new LangString("test", "en"));
        sut.add(new LangString("another", "en"));

        assertEquals(List.of(MultilingualString.create("test", "en"),
                             MultilingualString.create("another", "en")), sut.getValues());
    }

    @Test
    void addMergesValueIntoFirstMultilingualStringLackingItsLanguage() {
        sut.add(new LangString("test", "en"));
        sut.add(new LangString("another", "en"));
        sut.add(new LangString("test", "cs"));

        assertEquals(2, sut.getValues().size());
        final MultilingualString expectedFirst = MultilingualString.create("test", "en");
        expectedFirst.set("cs", "test");
        assertEquals(expectedFirst, sut.getValues().get(0));
        assertEquals(MultilingualString.create("another", "en"), sut.getValues().get(1));
    }

    @Test
    void addCreatesNewMultilingualStringForValueWithoutLanguage() {
        sut.add(new LangString("plain"));

        assertEquals(1, sut.getValues().size());
        assertTrue(sut.getValues().get(0).containsSimple());
        assertEquals("plain", sut.getValues().get(0).get());
    }

    @Test
    void addAppendsNewMultilingualStringForValueWithoutLanguageWhenExistingOnesContainSimpleValue() {
        sut.add(new LangString("one"));
        sut.add(new LangString("two"));

        assertEquals(List.of(MultilingualString.create("one", null),
                             MultilingualString.create("two", null)), sut.getValues());
    }

    @Test
    void addMergesValueWithLanguageIntoMultilingualStringCreatedForValueWithoutLanguage() {
        sut.add(new LangString("plain"));
        sut.add(new LangString("test", "en"));

        assertEquals(1, sut.getValues().size());
        final MultilingualString expected = MultilingualString.create("plain", null);
        expected.set("en", "test");
        assertEquals(expected, sut.getValues().get(0));
    }
}
