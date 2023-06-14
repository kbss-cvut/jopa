/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.jena.util;

import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.datatypes.TypeMapper;
import org.apache.jena.rdf.model.Literal;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.vocabulary.XSD;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.net.URL;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import static cz.cvut.kbss.jopa.datatype.DateTimeUtil.SYSTEM_OFFSET;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class JenaUtilsTest {

    private static final Assertion ASSERTION = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);

    @Test
    public void literalToValueReturnsTypeRepresentedByLiteral() {
        assertEquals(117, JenaUtils.literalToValue(ResourceFactory.createTypedLiteral(117)));
        assertEquals(true, JenaUtils.literalToValue(ResourceFactory.createTypedLiteral(true)));
        assertEquals("test", JenaUtils.literalToValue(ResourceFactory.createTypedLiteral("test")));
    }

    @Test
    public void literalToValueTranslatesLongLiteralToJavaLong() {
        assertEquals(117L, JenaUtils.literalToValue(ResourceFactory.createTypedLiteral(117L)));
    }

    @Test
    void literalToValueTransformsStringWithLanguageToLangString() {
        final Literal literal = ResourceFactory.createLangLiteral("test", "en");
        final Object result = JenaUtils.literalToValue(literal);
        assertThat(result, instanceOf(LangString.class));
        assertEquals("test", ((LangString) result).getValue());
        assertTrue(((LangString) result).getLanguage().isPresent());
        assertEquals("en", ((LangString) result).getLanguage().get());
    }

    @Test
    void valueToRdfNodeCreatesRdfLiteralFromOntoDriverLiteralWithLexicalFormAndDatatype() {
        final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = new cz.cvut.kbss.ontodriver.model.Literal("P1Y",
                                                                                                            XSD.duration.getURI());
        final RDFNode result = JenaUtils.valueToRdfNode(ASSERTION, new Value<>(ontoLiteral));
        assertTrue(result.isLiteral());
        assertEquals(ontoLiteral.getLexicalForm(), result.asLiteral().getLexicalForm());
        assertEquals(XSD.duration.getURI(), result.asLiteral().getDatatypeURI());
    }

    @Test
    void literalToValueTransformsXSDDatetimeToOffsetDateTime() {
        final Date date = new Date();
        final Calendar cal = GregorianCalendar.getInstance();
        cal.setTime(date);
        final Literal literal = ResourceFactory.createTypedLiteral(cal);
        assertEquals(XSD.dateTime.getURI(), literal.getDatatype().getURI());
        final Object result = JenaUtils.literalToValue(literal);
        assertThat(result, instanceOf(OffsetDateTime.class));
        final OffsetDateTime odtResult = (OffsetDateTime) result;
        // UTC due to the way Jena handles creation of dateTime literals from java.util.Date
        assertEquals(OffsetDateTime.ofInstant(date.toInstant(), ZoneId.ofOffset("UTC", ZoneOffset.UTC)), odtResult);
    }

    @Test
    void literalToValueTransformsCustomDatatypeLiteralToOntoDriverLiteral() {
        final Literal literal = ResourceFactory.createTypedLiteral("P1Y",
                                                                   TypeMapper.getInstance()
                                                                             .getTypeByName(XSD.gMonth.getURI()));
        final Object result = JenaUtils.literalToValue(literal);
        assertThat(result, instanceOf(cz.cvut.kbss.ontodriver.model.Literal.class));
        final cz.cvut.kbss.ontodriver.model.Literal literalResult = (cz.cvut.kbss.ontodriver.model.Literal) result;
        assertEquals(literal.getLexicalForm(), literalResult.getLexicalForm());
        assertEquals(literal.getDatatype().getURI(), literalResult.getDatatype());
    }

    @Test
    void valueToRdfNodeCreatesXsdDateTimeFromJavaUtilDateInstance() {
        final Instant instant = Instant.now().truncatedTo(ChronoUnit.MILLIS);
        final Date date = Date.from(instant);
        final RDFNode result = JenaUtils.valueToRdfNode(ASSERTION, new Value<>(date));
        assertTrue(result.isLiteral());
        assertEquals(instant.atOffset(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                     result.asLiteral().getLexicalForm());
        assertEquals(XSD.dateTime.getURI(), result.asLiteral().getDatatypeURI());
    }

    @Test
    void valueToRdfNodeCreatesXsdTimeFromLocalTimeInstance() {
        final LocalTime value = LocalTime.now().truncatedTo(ChronoUnit.MILLIS);
        final RDFNode result = JenaUtils.valueToRdfNode(ASSERTION, new Value<>(value));
        assertTrue(result.isLiteral());
        assertEquals(value.atOffset(SYSTEM_OFFSET).format(DateTimeFormatter.ISO_OFFSET_TIME),
                     result.asLiteral().getLexicalForm());
        assertEquals(XSD.time.getURI(), result.asLiteral().getDatatypeURI());
    }

    @Test
    void valueToRdfNodeCreatesXsdDateFromLocalDateInstance() {
        final LocalDate value = LocalDate.now();
        final RDFNode result = JenaUtils.valueToRdfNode(ASSERTION, new Value<>(value));
        assertTrue(result.isLiteral());
        assertEquals(value.format(DateTimeFormatter.ISO_DATE), result.asLiteral().getLexicalForm());
        assertEquals(XSD.date.getURI(), result.asLiteral().getDatatypeURI());
    }

    @Test
    void valueToRdfNodeCreatesXsdDurationFromJavaPeriodInstance() {
        final Period value = Period.ofDays(Generator.randomInt(10000));
        final RDFNode result = JenaUtils.valueToRdfNode(ASSERTION, new Value<>(value));
        assertTrue(result.isLiteral());
        assertEquals(value.toString(), result.asLiteral().getLexicalForm());
        assertEquals(XSD.duration.getURI(), result.asLiteral().getDatatypeURI());
    }

    @Test
    void valueToRdfNodeCreatesXsdIntegerFromJavaBigInteger() {
        final BigInteger value = BigInteger.valueOf(System.currentTimeMillis());
        final RDFNode result = JenaUtils.valueToRdfNode(ASSERTION, new Value<>(value));
        assertTrue(result.isLiteral());
        assertEquals(value.toString(), result.asLiteral().getLexicalForm());
        assertEquals(XSD.integer.getURI(), result.asLiteral().getDatatypeURI());
    }

    @Test
    void valueToRdfNodeCreatesXsdDecimalFromJavaBigDecimal() {
        final BigDecimal value = BigDecimal.valueOf(Math.PI);
        final RDFNode result = JenaUtils.valueToRdfNode(ASSERTION, new Value<>(value));
        assertTrue(result.isLiteral());
        assertEquals(value.toString(), result.asLiteral().getLexicalForm());
        assertEquals(XSD.decimal.getURI(), result.asLiteral().getDatatypeURI());
    }

    /**
     * Bug #113
     */
    @Test
    void valueToRdfNodeSupportsLiteralsWithCustomExplicitDatatype() {
        // Ensures that non-built-in datatypes are supported as well
        final cz.cvut.kbss.ontodriver.model.Literal jopaLiteral =
                cz.cvut.kbss.ontodriver.model.Literal.from("test", "http://www.w3.org/ns/csvw#uriTemplate");
        final RDFNode result = JenaUtils.valueToRdfNode(ASSERTION, new Value<>(jopaLiteral));
        assertTrue(result.isLiteral());
        assertEquals(jopaLiteral.getLexicalForm(), result.asLiteral().getLexicalForm());
        assertEquals(jopaLiteral.getDatatype(), result.asLiteral().getDatatypeURI());
    }

    @Test
    void valueToRdfNodeCreatesResourceForIdentifierTypes() throws Exception {
        final URI uriValue = Generator.generateUri();
        final RDFNode uriResult = JenaUtils.valueToRdfNode(ASSERTION, new Value<>(uriValue));
        assertTrue(uriResult.isURIResource());
        assertEquals(ResourceFactory.createResource(uriValue.toString()), uriResult);
        final URL urlValue = Generator.generateUri().toURL();
        final RDFNode urlResult = JenaUtils.valueToRdfNode(ASSERTION, new Value<>(urlValue));
        assertTrue(urlResult.isURIResource());
        assertEquals(ResourceFactory.createResource(urlValue.toString()), urlResult);
    }

    @Test
    void valueToRdfNodeCreatesStringLiteralForStringUri() {
        final String value = Generator.generateUri().toString();
        final RDFNode result = JenaUtils.valueToRdfNode(
                Assertion.createAnnotationPropertyAssertion(Generator.generateUri(), "en", false), new Value<>(value));
        assertTrue(result.isLiteral());
        assertEquals(ResourceFactory.createLangLiteral(value, "en"), result);
    }
}
