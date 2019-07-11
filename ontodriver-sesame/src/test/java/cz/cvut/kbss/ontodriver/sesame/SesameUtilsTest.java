/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.sesame.environment.Generator;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.XMLSchema;
import org.eclipse.rdf4j.sail.memory.MemoryStore;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SesameUtilsTest {

    private static final String LANG = "en";

    private static ValueFactory vf;

    private static MemoryStore memoryStore;

    private enum Severity {
        LOW, MEDIUM
    }

    @BeforeAll
    static void setUpBeforeClass() {
        memoryStore = new MemoryStore();
        memoryStore.initialize();
        vf = memoryStore.getValueFactory();
    }

    @AfterAll
    static void tearDownAfterClass() {
        memoryStore.shutDown();
    }

    @Test
    void enumLiteralIsReturnedAsStringValue() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Object result = SesameUtils.getDataPropertyValue(literal);
        assertEquals(Severity.LOW.toString(), result);
    }

    @Test
    void doesLanguageMatchReturnsFalseForNonMatchingLanguageTag() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        assertFalse(SesameUtils.doesLanguageMatch(literal, "cs"));
    }

    @Test
    void doesLanguageMatchReturnsTrueForMatchingLanguageTag() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        assertTrue(SesameUtils.doesLanguageMatch(literal, LANG));
    }

    @Test
    void doesLanguageMatchReturnsTrueWhenNoLanguageIsSpecified() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        assertTrue(SesameUtils.doesLanguageMatch(literal, (String) null));
    }

    @Test
    void doesLanguageMatchReturnsTrueWhenLiteralHasNoLanguage() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString());
        assertTrue(SesameUtils.doesLanguageMatch(literal, LANG));
    }

    @Test
    void doesLanguageMatchForAssertionReturnsFalseForNonMatchingLanguageTag() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), "cs", false);
        assertFalse(SesameUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void doesLanguageMatchForAssertionReturnsTrueForMatchingLanguageTag() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), LANG, false);
        assertTrue(SesameUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void doesLanguageMatchForAssertionReturnsTrueWhenAssertionHasNoLanguage() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), false);
        assertTrue(SesameUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void doesLanguageMatchForAssertionReturnsTrueWhenLiteralHasNoLanguage() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString());
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), LANG, false);
        assertTrue(SesameUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void enumValueIsReturnedAsStringLiteral() {
        final Literal literal = SesameUtils.createDataPropertyLiteral(Severity.MEDIUM, LANG, vf);
        assertNotNull(literal);
        assertEquals(Severity.MEDIUM.toString(), literal.stringValue());
        assertTrue(literal.getDatatype() == null || literal.getDatatype().equals(XMLSchema.STRING));
    }

    @Test
    void createDataPropertyLiteralAttachesLanguageTagToStringLiteral() {
        final String value = "literal";
        final Literal result = SesameUtils.createDataPropertyLiteral(value, LANG, vf);
        assertTrue(result.getLanguage().isPresent());
        assertEquals(LANG, result.getLanguage().get());
        assertEquals(value, result.stringValue());
    }

    @Test
    void createDataPropertyLiteralCreatesStringWithoutLanguageTagWhenNullIsPassedIn() {
        final String value = "literal";
        final Literal result = SesameUtils.createDataPropertyLiteral(value, null, vf);
        assertFalse(result.getLanguage().isPresent());
        assertEquals(value, result.stringValue());
    }
}