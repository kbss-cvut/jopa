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

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class JenaUtilsTest {

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
        final cz.cvut.kbss.ontodriver.model.Literal ontoLiteral = new cz.cvut.kbss.ontodriver.model.Literal("P1Y", XSD.duration.getURI());
        final Assertion a = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final RDFNode result = JenaUtils.valueToRdfNode(a, new Value<>(ontoLiteral));
        assertThat(result, instanceOf(Literal.class));
        assertEquals(ontoLiteral.getLexicalForm(), result.asLiteral().getLexicalForm());
        assertEquals(XSD.duration.getURI(), result.asLiteral().getDatatype().getURI());
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
        final Literal literal = ResourceFactory.createTypedLiteral("P1Y", TypeMapper.getInstance().getTypeByName(XSD.gMonth.getURI()));
        final Object result = JenaUtils.literalToValue(literal);
        assertThat(result, instanceOf(cz.cvut.kbss.ontodriver.model.Literal.class));
        final cz.cvut.kbss.ontodriver.model.Literal literalResult = (cz.cvut.kbss.ontodriver.model.Literal) result;
        assertEquals(literal.getLexicalForm(), literalResult.getLexicalForm());
        assertEquals(literal.getDatatype().getURI(), literalResult.getDatatype());
    }

    @Test
    void valueToRdfNodeCreatesXsdDateTimeFromJavaUtilDateInstance() {
        final Instant instant = Instant.now().truncatedTo(ChronoUnit.SECONDS);
        final Date date = Date.from(instant);
        final Assertion a = Assertion.createDataPropertyAssertion(Generator.generateUri(), false);
        final RDFNode result = JenaUtils.valueToRdfNode(a, new Value<>(date));
        assertThat(result, instanceOf(Literal.class));
        final TimeZone tz = TimeZone.getTimeZone("UTC");
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
        df.setTimeZone(tz);
        assertEquals(df.format(date), result.asLiteral().getLexicalForm());
        assertEquals(XSD.dateTime.getURI(), result.asLiteral().getDatatype().getURI());
    }
}
