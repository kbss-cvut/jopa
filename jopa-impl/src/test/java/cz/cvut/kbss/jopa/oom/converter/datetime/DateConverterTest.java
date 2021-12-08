package cz.cvut.kbss.jopa.oom.converter.datetime;

import cz.cvut.kbss.jopa.exception.IncompatibleDatatypeException;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class DateConverterTest {

    private static DatatypeFactory datatypeFactory;

    private final DateConverter sut = new DateConverter();

    @BeforeAll
    static void setUpBeforeAll() throws Exception {
        datatypeFactory = DatatypeFactory.newInstance();
    }

    @ParameterizedTest
    @MethodSource("generateDates")
    void convertToAttributeParsesRDFLiteralsToDate(String argument) {
        final Date result = sut.convertToAttribute(new Literal(argument, XSD.DATETIME));
        final XMLGregorianCalendar calendar = datatypeFactory.newXMLGregorianCalendar(argument);
        assertEquals(calendar.toGregorianCalendar().getTime(), result);
    }

    static Stream<String> generateDates() {
        return Stream.of("2020-06-29T08:16:03.463+02:00", "2019-12-12T20:46:45.686+01:00", "2001-10-26T21:32:52", "2001-10-26T19:32:52+00:00");
    }

    @Test
    void convertToAttributeThrowsIncompatibleDatatypeExceptionForLiteralNotDateTime() {
        assertThrows(IncompatibleDatatypeException.class, () -> sut.convertToAttribute(new Literal("117", XSD.INT)));
    }

    @Test
    void convertToAxiomReturnsXSDDateTimeValueInISOFormat() {
        final Date test = new Date();
        final Literal result = sut.convertToAxiomValue(test);
        assertEquals(XSD.DATETIME, result.getDatatype());
        final String expected = ZonedDateTime.ofInstant(test.toInstant(), ZoneOffset.UTC).format(DateTimeFormatter.ISO_INSTANT);
        assertEquals(expected, result.getLexicalForm());
    }
}
