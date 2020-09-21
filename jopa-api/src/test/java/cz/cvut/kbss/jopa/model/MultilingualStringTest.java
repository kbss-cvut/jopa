package cz.cvut.kbss.jopa.model;

import org.junit.jupiter.api.Test;

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
        assertEquals(one.getValue(), two.getValue());
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
        assertFalse(sut.getValue().containsKey(LANGUAGE));
    }

    @Test
    void toStringOutputsInternalValue() {
        final MultilingualString sut = MultilingualString.create(VALUE, LANGUAGE);
        assertEquals(sut.getValue().toString(), sut.toString());
    }
}
