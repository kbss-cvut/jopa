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
package cz.cvut.kbss.ontodriver.rdf4j.util;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.rdf4j.environment.Generator;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class Rdf4jUtilsTest {

    private static final String LANG = "en";

    private static final ValueFactory vf = SimpleValueFactory.getInstance();

    private enum Severity {
        LOW, MEDIUM
    }

    @Test
    void enumLiteralIsReturnedAsStringValue() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString());
        final Object result = Rdf4jUtils.getLiteralValue(literal);
        assertEquals(Severity.LOW.toString(), result);
    }

    @Test
    void doesLanguageMatchForAssertionReturnsFalseForNonMatchingLanguageTag() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), "cs", false);
        assertFalse(Rdf4jUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void doesLanguageMatchForAssertionReturnsTrueForMatchingLanguageTag() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), LANG, false);
        assertTrue(Rdf4jUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void doesLanguageMatchForAssertionReturnsTrueWhenAssertionHasNoLanguage() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString(), LANG);
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), false);
        assertTrue(Rdf4jUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void doesLanguageMatchForAssertionReturnsTrueWhenLiteralHasNoLanguage() {
        final Literal literal = vf.createLiteral(Severity.LOW.toString());
        final Assertion assertion = Assertion.createPropertyAssertion(Generator.generateUri(), LANG, false);
        assertTrue(Rdf4jUtils.doesLanguageMatch(literal, assertion));
    }

    @Test
    void enumValueIsReturnedAsStringLiteral() {
        final Literal literal = Rdf4jUtils.createLiteral(Severity.MEDIUM, LANG, vf);
        assertNotNull(literal);
        assertEquals(Severity.MEDIUM.toString(), literal.stringValue());
        assertEquals(literal.getDatatype(), XSD.STRING);
    }

    @Test
    void createLiteralAttachesLanguageTagToStringLiteral() {
        final String value = "literal";
        final Literal result = Rdf4jUtils.createLiteral(value, LANG, vf);
        assertTrue(result.getLanguage().isPresent());
        assertEquals(LANG, result.getLanguage().get());
        assertEquals(value, result.stringValue());
        assertEquals(RDF.LANGSTRING, result.getDatatype());
    }

    @Test
    void createLiteralCreatesStringWithoutLanguageTagWhenNullIsPassedIn() {
        final String value = "literal";
        final Literal result = Rdf4jUtils.createLiteral(value, null, vf);
        assertFalse(result.getLanguage().isPresent());
        assertEquals(value, result.stringValue());
        assertEquals(XSD.STRING, result.getDatatype());
    }

    @Test
    void getLiteralValueReturnsLangStringForLangStringLiteral() {
        final Literal literal = vf.createLiteral("test", LANG);
        final Object result = Rdf4jUtils.getLiteralValue(literal);
        assertThat(result, instanceOf(LangString.class));
        final LangString lsResult = (LangString) result;
        assertEquals(literal.stringValue(), lsResult.getValue());
        assertTrue(lsResult.getLanguage().isPresent());
        assertEquals(literal.getLanguage(), lsResult.getLanguage());
    }

    @Test
    void createLiteralCreatesRDFLangStringWithLanguageSpecifiedByOntoDriverLangString() {
        final LangString ls = new LangString("test", LANG);
        final Literal result = Rdf4jUtils.createLiteral(ls, null, vf);
        assertEquals(ls.getValue(), result.stringValue());
        assertTrue(result.getLanguage().isPresent());
        assertEquals(ls.getLanguage(), result.getLanguage());
        assertEquals(RDF.LANGSTRING, result.getDatatype());
    }

    @Test
    void createLiteralCreatesSimpleLiteralFromOntoDriverLangStringWithoutLanguage() {
        final LangString ls = new LangString("test");
        final Literal result = Rdf4jUtils.createLiteral(ls, null, vf);
        assertEquals(ls.getValue(), result.stringValue());
        assertFalse(result.getLanguage().isPresent());
        assertEquals(XSD.STRING, result.getDatatype());
    }

    @Test
    void createLiteralCreatesRdf4jLiteralWhenOntoDriverLiteralWithLexicalFormAndDatatypeIsProvided() {
        final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = new cz.cvut.kbss.ontodriver.model.Literal("P1Y",
                XSD.DURATION.stringValue());
        final Literal result = Rdf4jUtils.createLiteral(ontoLiteral, null, vf);
        assertEquals(ontoLiteral.getLexicalForm(), result.getLabel());
        assertEquals(XSD.DURATION, result.getDatatype());
    }

    @Test
    void getLiteralValueReturnsOntoDriverLiteralForUnsupportedDatatype() {
        final Literal literal = vf.createLiteral("P1Y", XSD.QNAME);
        final Object result = Rdf4jUtils.getLiteralValue(literal);
        assertThat(result, instanceOf(cz.cvut.kbss.ontodriver.model.Literal.class));
        final cz.cvut.kbss.ontodriver.model.Literal literalResult = (cz.cvut.kbss.ontodriver.model.Literal) result;
        assertEquals(literal.getLabel(), literalResult.getLexicalForm());
        assertEquals(literal.getDatatype().stringValue(), literalResult.getDatatype());
    }

    @Test
    void createLiteralReturnsXsdIntegerForBigInteger() {
        final BigInteger value = BigInteger.valueOf(System.currentTimeMillis());
        final Literal result = Rdf4jUtils.createLiteral(value, null, vf);
        assertEquals(value.toString(), result.getLabel());
        assertEquals(XSD.INTEGER, result.getDatatype());
    }

    @Test
    void createLiteralReturnsXsdDecimalForBigDecimal() {
        final BigDecimal value = BigDecimal.valueOf(Math.PI);
        final Literal result = Rdf4jUtils.createLiteral(value, null, vf);
        assertEquals(value.toString(), result.getLabel());
        assertEquals(XSD.DECIMAL, result.getDatatype());
    }

    @Test
    void createLiteralReturnsXsdDateTimeAtUTCForDate() {
        final Date value = new Date();
        final Literal result = Rdf4jUtils.createLiteral(value, null, vf);
        assertEquals(value.toInstant().atOffset(ZoneOffset.UTC)
                .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), result.getLabel());
        assertEquals(XSD.DATETIME, result.getDatatype());
    }

    @Test
    void creatLiteralCreatesDateTimeLiteralWithOffsetForLocalDateTime() {
        final LocalDateTime value = LocalDateTime.now();
        final Literal literal = Rdf4jUtils.createLiteral(value, null, vf);
        ZoneOffset offset = ZoneId.systemDefault().getRules().getOffset(value);
        OffsetDateTime offsetDateTime = OffsetDateTime.of(value.toLocalDate(), value.toLocalTime(), offset);
        assertEquals(offsetDateTime.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), literal.getLabel());
        assertEquals(XSD.DATETIME, literal.getDatatype());
    }

    @Test
    void createLiteralCreatesDurationLiteralForJavaDuration() {
        final Duration value = Duration.ofSeconds(Generator.randomPositiveInt(1000));
        final Literal literal = Rdf4jUtils.createLiteral(value, null, vf);
        assertEquals(value.toString(), literal.getLabel());
        assertEquals(XSD.DURATION, literal.getDatatype());
    }
}
