/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
