package cz.cvut.kbss.ontodriver.model;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class LangStringTest {

    @Test
    void equalsReturnsTrueWhenValueAndLanguageMatch() {
        assertEquals(new LangString("test", "cs"), new LangString("test", "cs"));
        assertNotEquals(new LangString("test", "cs"), new LangString("test", "en"));
        assertNotEquals(new LangString("test", "cs"), new LangString("test"));
        assertEquals(new LangString("test"), new LangString("test", null));
        final LangString s = new LangString("test", "cs");
        assertEquals(s, s);
    }

    @Test
    void hashCodeIsEqualWhenValueAndLanguageMatch() {
        assertEquals(new LangString("test", "cs").hashCode(), new LangString("test", "cs").hashCode());
        assertNotEquals(new LangString("test", "cs").hashCode(), new LangString("test", "en").hashCode());
        assertNotEquals(new LangString("test", "cs").hashCode(), new LangString("test").hashCode());
        assertEquals(new LangString("test").hashCode(), new LangString("test", null).hashCode());
    }

    @Test
    void getLanguageReturnsEmptyOptionalForLangStringWithoutLanguage() {
        assertFalse(new LangString("tes").getLanguage().isPresent());
    }
}
