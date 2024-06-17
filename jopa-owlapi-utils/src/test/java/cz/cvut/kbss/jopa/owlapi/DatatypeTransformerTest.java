/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Date;

import static cz.cvut.kbss.jopa.datatype.DateTimeUtil.SYSTEM_OFFSET;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DatatypeTransformerTest {

    private static final OWLDataFactory DATA_FACTORY = new OWLDataFactoryImpl();

    @Test
    void transformObjectToOwlLiteralReturnsRdfLangStringForOntoDriverLangStringWithLanguage() {
        final LangString ls = new LangString("test", "en");
        final OWLLiteral result = DatatypeTransformer.transform(ls, null);
        assertEquals(OWL2Datatype.RDF_LANG_STRING.getDatatype(DATA_FACTORY), result.getDatatype());
        assertEquals(ls.getValue(), result.getLiteral());
        assert ls.getLanguage().isPresent();
        assertEquals(ls.getLanguage().get(), result.getLang());
    }

    @Test
    void transformObjectToOwlLiteralReturnsSimpleLiteralForOntoDriverLangStringWithoutLanguage() {
        final LangString ls = new LangString("test");
        final OWLLiteral result = DatatypeTransformer.transform(ls, null);
        assertEquals(OWL2Datatype.XSD_STRING.getDatatype(DATA_FACTORY), result.getDatatype());
        assertEquals(ls.getValue(), result.getLiteral());
        assertFalse(result.hasLang());
        assertTrue(result.getLang().isEmpty());
    }

    @Test
    void transformObjectToOwlLiteralSupportsMappingOntoLiteralWithLexicalFormAndDatatype() {
        final Literal ontoLiteral = new Literal("P1Y", XSD.DURATION);
        final OWLLiteral result = DatatypeTransformer.transform(ontoLiteral, null);
        assertEquals(ontoLiteral.getLexicalForm(), result.getLiteral());
        assertEquals(XSD.DURATION, result.getDatatype().toStringID());
    }

    @Test
    void transformObjectToOwlLiteralReturnsXsdDateTimeAtUTCOffsetForDate() {
        final Instant instant = Instant.now().truncatedTo(ChronoUnit.MILLIS);
        final Date value = Date.from(instant);
        final OWLLiteral result = DatatypeTransformer.transform(value, null);
        assertEquals(instant.atOffset(ZoneOffset.UTC).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
                result.getLiteral());
        assertEquals(XSD.DATETIME, result.getDatatype().toStringID());
    }

    @Test
    void transformObjectToOwlLiteralReturnsXsdDateTimeAtSystemOffsetForLocalDateTime() {
        final LocalDateTime value = LocalDateTime.now();
        final OWLLiteral result = DatatypeTransformer.transform(value, null);
        assertEquals(value.atOffset(SYSTEM_OFFSET).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), result.getLiteral());
        assertEquals(XSD.DATETIME, result.getDatatype().toStringID());
    }

    @Test
    void transformObjectToOwlLiteralReturnsXsdTimeAtSystemOffsetForLocalTime() {
        final LocalTime value = LocalTime.now();
        final OWLLiteral result = DatatypeTransformer.transform(value, null);
        assertEquals(value.atOffset(SYSTEM_OFFSET).format(DateTimeFormatter.ISO_OFFSET_TIME), result.getLiteral());
        assertEquals(XSD.TIME, result.getDatatype().toStringID());
    }

    @Test
    void transformObjectToOwlLiteralReturnsXsdDateForLocalDate() {
        final LocalDate value = LocalDate.now();
        final OWLLiteral result = DatatypeTransformer.transform(value, null);
        assertEquals(value.format(DateTimeFormatter.ISO_DATE), result.getLiteral());
        assertEquals(XSD.DATE, result.getDatatype().toStringID());
    }

    @Test
    void transformObjectToOwlLiteralReturnsXsdDurationForJavaDuration() {
        final Duration value = Duration.ofSeconds(158);
        final OWLLiteral result = DatatypeTransformer.transform(value, null);
        assertEquals(value.toString(), result.getLiteral());
        assertEquals(XSD.DURATION, result.getDatatype().toStringID());
    }
}
