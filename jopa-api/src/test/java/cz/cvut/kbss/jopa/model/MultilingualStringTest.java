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
package cz.cvut.kbss.jopa.model;

import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.Set;

import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class MultilingualStringTest {

    private static final String VALUE = "test";
    private static final String LANGUAGE = "en";

    @Test
    void staticCreateMethodInitializesStringWithSpecifiedValueAndLanguage() {
        final MultilingualString sut = MultilingualString.create(VALUE, LANGUAGE);
        assertNotNull(sut);
        assertEquals(VALUE, sut.get(LANGUAGE));
    }

    @Test
    void setSetsValueInSpecifiedLanguage() {
        final MultilingualString sut = new MultilingualString();
        sut.set(LANGUAGE, VALUE);
        assertEquals(VALUE, sut.get(LANGUAGE));
    }

    @Test
    void setOverridesPreexistingValueForSpecifiedLanguage() {
        final MultilingualString sut = new MultilingualString();
        sut.set(LANGUAGE, VALUE);
        final String newValue = "exam";
        sut.set(LANGUAGE, newValue);
        assertEquals(newValue, sut.get(LANGUAGE));
    }

    @Test
    void setWithoutLanguageSetsSimpleLiteralValue() {
        final MultilingualString sut = new MultilingualString();
        sut.set(VALUE);
        assertEquals(VALUE, sut.get(null));
    }

    @Test
    void getWithNullLanguageReturnsSimpleLiteralValue() {
        final MultilingualString sut = new MultilingualString();
        sut.set(VALUE);
        assertEquals(VALUE, sut.get(null));
    }

    @Test
    void getWithNullLanguageReturnsAnyExistingLanguageTaggedValueWhenSimpleLiteralIsNotPresent() {
        final MultilingualString sut = new MultilingualString();
        sut.set(LANGUAGE, VALUE);
        assertEquals(VALUE, sut.get(null));
    }

    @Test
    void getWithoutArgumentReturnsSimpleLiteralValue() {
        final MultilingualString sut = new MultilingualString();
        sut.set(VALUE);
        assertEquals(VALUE, sut.get());
    }

    @Test
    void getWithoutArgumentReturnsAnyExistingLanguageTaggedValueWhenSimpleLiteralIsNotPresent() {
        final MultilingualString sut = new MultilingualString();
        sut.set(LANGUAGE, VALUE);
        assertEquals(VALUE, sut.get());
    }

    @Test
    void containsReturnsTrueForExistingLanguageValue() {
        final MultilingualString sut = new MultilingualString();
        sut.set(LANGUAGE, VALUE);
        assertTrue(sut.contains(LANGUAGE));
        assertFalse(sut.contains("cs"));
    }

    @Test
    void containsReturnsTrueForNullArgumentWhenAtLeastOneLanguageTaggedValueExists() {
        final MultilingualString sut = new MultilingualString();
        sut.set(LANGUAGE, VALUE);
        assertTrue(sut.contains(null));
    }

    @Test
    void getLanguagesReturnsSetOfLanguagesPresentInInstance() {
        final MultilingualString sut = new MultilingualString();
        sut.set(LANGUAGE, VALUE);
        sut.set("test");
        final Set<String> result = sut.getLanguages();
        assertEquals(2, result.size());
        assertThat(result, hasItems(LANGUAGE, null));
    }

    @Test
    void equalityWorksBasedOnTranslations() {
        final MultilingualString one = MultilingualString.create(VALUE, LANGUAGE);
        one.set(VALUE);
        final MultilingualString two = MultilingualString.create(VALUE, LANGUAGE);
        two.set(VALUE);
        assertEquals(one, two);
        assertEquals(one.hashCode(), two.hashCode());
    }

    @Test
    void getReturnsNullWhenValueInSpecifiedLanguageIsNotAvailable() {
        final MultilingualString sut = new MultilingualString();
        assertNull(sut.get(LANGUAGE));
    }

    @Test
    void getWithNullArgumentReturnsNullWhenNeitherSimpleLiteralNorTranslationsAreAvailable() {
        final MultilingualString sut = new MultilingualString();
        assertNull(sut.get());
    }

    @Test
    void containsSimpleReturnsTrueForSimpleLiteral() {
        final MultilingualString sut = MultilingualString.create(VALUE, LANGUAGE);
        assertFalse(sut.containsSimple());
        sut.set("test");
        assertTrue(sut.containsSimple());
    }

    @Test
    void isEmptyReturnsTrueForMissingTranslations() {
        final MultilingualString sut = new MultilingualString();
        assertTrue(sut.isEmpty());
        sut.set("test");
        assertFalse(sut.isEmpty());
    }

    @Test
    void removeRemovesTranslationInSpecifiedLanguage() {
        final MultilingualString sut = MultilingualString.create(VALUE, LANGUAGE);
        assertTrue(sut.contains(LANGUAGE));
        sut.remove(LANGUAGE);
        assertFalse(sut.contains(LANGUAGE));
        assertFalse(sut.containsKey(LANGUAGE));
    }

    @Test
    void setTransformsJsonLdNoneToNullKey() {
        final MultilingualString sut = new MultilingualString();
        sut.set("@none", "Test");
        assertEquals("Test", sut.get());
        assertEquals("Test", sut.get(null));
        assertTrue(sut.containsSimple());
        assertFalse(sut.containsKey("@none"));
    }

    @Test
    void putTransformsJsonLdNoneToNullKey() {
        final MultilingualString sut = new MultilingualString();
        sut.put("@none", "Test");
        assertEquals("Test", sut.get());
        assertEquals("Test", sut.get(null));
        assertTrue(sut.containsSimple());
        assertFalse(sut.containsKey("@none"));
    }

    @Test
    void removeTransformsJsonLdNoneToNullKey() {
        final MultilingualString sut = new MultilingualString();
        sut.putAll(Map.of("en", "English test", "cs", "Český test"));
        sut.put(null, "Simple literal");
        sut.remove("@none");
        assertFalse(sut.containsValue("Simple literal"));
    }

    @Test
    void containsTransformsJsonLdNoneToNullKey() {
        final MultilingualString sut = new MultilingualString();
        sut.putAll(Map.of("en", "English test", "cs", "Český test"));
        sut.put(null, "Simple literal");
        assertTrue(sut.contains("@none"));
    }

    @Test
    void getTransformsJsonLdNoneToNullKey() {
        final MultilingualString sut = new MultilingualString();
        sut.putAll(Map.of("en", "English test", "cs", "Český test"));
        sut.put(null, "Simple literal");
        assertEquals("Simple literal", sut.get("@none"));
        assertEquals("Simple literal", sut.get(null));
    }
}
