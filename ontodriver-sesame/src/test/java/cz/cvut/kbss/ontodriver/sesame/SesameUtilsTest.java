/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.sesame.environment.Generator;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class SesameUtilsTest {

    private static final String LANG = "en";

    private static final ValueFactory vf = SimpleValueFactory.getInstance();

    private enum Severity {
        LOW, MEDIUM
    }

    @Test
    void enumLiteralIsReturnedAsStringValue() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString());
        final Object result = SesameUtils.getDataPropertyValue(literal);
        assertEquals(Severity.LOW.toString(), result);
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
        final Literal literal = SesameUtils.createLiteral(Severity.MEDIUM, LANG, vf);
        assertNotNull(literal);
        assertEquals(Severity.MEDIUM.toString(), literal.stringValue());
        assertEquals(literal.getDatatype(), XSD.STRING);
    }

    @Test
    void createLiteralAttachesLanguageTagToStringLiteral() {
        final String value = "literal";
        final Literal result = SesameUtils.createLiteral(value, LANG, vf);
        assertTrue(result.getLanguage().isPresent());
        assertEquals(LANG, result.getLanguage().get());
        assertEquals(value, result.stringValue());
        assertEquals(RDF.LANGSTRING, result.getDatatype());
    }

    @Test
    void createLiteralCreatesStringWithoutLanguageTagWhenNullIsPassedIn() {
        final String value = "literal";
        final Literal result = SesameUtils.createLiteral(value, null, vf);
        assertFalse(result.getLanguage().isPresent());
        assertEquals(value, result.stringValue());
        assertEquals(XSD.STRING, result.getDatatype());
    }

    @Test
    void getDataPropertyValueReturnsLangStringForLangStringLiteral() {
        final Literal literal = vf.createLiteral("test", LANG);
        final Object result = SesameUtils.getDataPropertyValue(literal);
        assertThat(result, instanceOf(LangString.class));
        final LangString lsResult = (LangString) result;
        assertEquals(literal.stringValue(), lsResult.getValue());
        assertTrue(lsResult.getLanguage().isPresent());
        assertEquals(literal.getLanguage(), lsResult.getLanguage());
    }

    @Test
    void createLiteralCreatesRDFLangStringWithLanguageSpecifiedByOntoDriverLangString() {
        final LangString ls = new LangString("test", LANG);
        final Literal result = SesameUtils.createLiteral(ls, null, vf);
        assertEquals(ls.getValue(), result.stringValue());
        assertTrue(result.getLanguage().isPresent());
        assertEquals(ls.getLanguage(), result.getLanguage());
        assertEquals(RDF.LANGSTRING, result.getDatatype());
    }

    @Test
    void createLiteralCreatesSimpleLiteralFromOntoDriverLangStringWithoutLanguage() {
        final LangString ls = new LangString("test");
        final Literal result = SesameUtils.createLiteral(ls, null, vf);
        assertEquals(ls.getValue(), result.stringValue());
        assertFalse(result.getLanguage().isPresent());
        assertEquals(XSD.STRING, result.getDatatype());
    }

    @Test
    void createLiteralCreatesSesameLiteralWhenOntoDriverLiteralWithLexicalFormAndDatatypeIsProvided() {
        final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = new cz.cvut.kbss.ontodriver.model.Literal("P1Y", XSD.DURATION.stringValue());
        final Literal result = SesameUtils.createLiteral(ontoLiteral, null, vf);
        assertEquals(ontoLiteral.getLexicalForm(), result.getLabel());
        assertEquals(XSD.DURATION, result.getDatatype());
    }
}
