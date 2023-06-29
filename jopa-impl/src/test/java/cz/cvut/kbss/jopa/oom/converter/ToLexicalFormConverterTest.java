/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.oom.converter;

import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ToLexicalFormConverterTest {

    private final ToLexicalFormConverter sut = new ToLexicalFormConverter();

    @Test
    void convertToAttributeReturnsStringValueOfSpecifiedObject() {
        final Integer value = 117;
        assertTrue(sut.supportsAxiomValueType(value.getClass()));
        final String result = sut.convertToAttribute(value);
        assertEquals(value.toString(), result);
    }

    @Test
    void supportsAxiomValueTypeReturnsFalseForNamedResource() {
        assertFalse(sut.supportsAxiomValueType(NamedResource.class));
    }

    @Test
    void convertToAxiomValueReturnsSpecifiedString() {
        final String value = "test";
        assertEquals(value, sut.convertToAxiomValue(value));
    }

    @Test
    void convertToAttributeReturnsLexicalFormOfLangString() {
        final LangString langString = new LangString("test", "en");
        assertEquals(langString.getValue(), sut.convertToAttribute(langString));
    }
}
