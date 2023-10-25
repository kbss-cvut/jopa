/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.owlapi.util;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.owlapi.environment.Generator;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class OwlapiUtilsTest {

    private static final String LANG = "en";

    private final OWLDataFactory dataFactory = new OWLDataFactoryImpl();

    @Test
    void doesLanguageMatchForAssertionReturnsTrueWhenLanguageTagMatches() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test", LANG);
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), LANG, false);
        assertTrue(OwlapiUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void doesLanguageMatchForAssertionReturnsFalseWhenLanguageTagDoesNotMatch() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test", LANG);
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), "cs", false);
        assertFalse(OwlapiUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void doesLanguageMatchForAssertionsReturnsTrueWhenLanguageIsNotSpecifiedOnAssertion() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test", LANG);
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), false);
        assertTrue(OwlapiUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void doesLanguageMatchForAssertionReturnsTrueWhenLiteralHasNoLanguageTag() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test");
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), LANG, false);
        assertTrue(OwlapiUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void doesLanguageMatchForAssertionReturnsTrueForNonStringLiteral() {
        final OWLLiteral literal = dataFactory.getOWLLiteral(117);
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), LANG, false);
        assertTrue(OwlapiUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void createOwlLiteralFromValueCreatesSimpleLiteralFromStringWithoutLanguageTag() {
        final OWLLiteral result = OwlapiUtils.createOWLLiteralFromValue("test", null);
        assertFalse(result.hasLang());
        assertEquals(OWL2Datatype.XSD_STRING.getDatatype(dataFactory), result.getDatatype());
    }

    @Test
    void createOwlLiteralFromValueCreatesLangStringFromStringWithLanguageTag() {
        final OWLLiteral result = OwlapiUtils.createOWLLiteralFromValue("test", LANG);
        assertTrue(result.hasLang());
        assertEquals(OWL2Datatype.RDF_LANG_STRING.getDatatype(dataFactory), result.getDatatype());
    }

    @Test
    void owlLiteralToValueReturnLangStringForStringLiteralWithLanguage() {
        final OWLLiteral literal = dataFactory.getOWLLiteral("test", LANG);
        final Object result = OwlapiUtils.owlLiteralToValue(literal);
        assertThat(result, instanceOf(LangString.class));
        assertEquals("test", ((LangString) result).getValue());
        assertTrue(((LangString) result).getLanguage().isPresent());
        assertEquals(LANG, ((LangString) result).getLanguage().get());
    }
}
