package cz.cvut.kbss.jopa.oom.converter.datetime;

import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;

import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

class ZonedDateTimeConverterTest {

    private final ZonedDateTimeConverter sut = new ZonedDateTimeConverter();

    @Test
    void convertToAttributeTransformsJavaOffsetDateTimeToZonedDateTime() {
        final OffsetDateTime value = OffsetDateTime.now();
        final ZonedDateTime result = sut.convertToAttribute(value);
        assertEquals(value.toZonedDateTime(), result);
    }

    @Test
    void convertToAttributeTransformsLiteralToZonedDateTime() {
        final OffsetDateTime value = OffsetDateTime.now();
        final Literal literal = Literal.from(value.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME), XSD.DATETIME);
        final ZonedDateTime result = sut.convertToAttribute(literal);
        assertEquals(value.toZonedDateTime(), result);
    }

    @Test
    void supportsAxiomValueTypeReturnsTrueForOffsetDateTime() {
        assertTrue(sut.supportsAxiomValueType(OffsetDateTime.class));
        assertFalse(sut.supportsAxiomValueType(Date.class));
    }

    @Test
    void supportsAxiomValueTypeReturnsTrueForLiteral() {
        assertTrue(sut.supportsAxiomValueType(Literal.class));
    }
}