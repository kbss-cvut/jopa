package cz.cvut.kbss.jopa.oom.converter.datetime;

import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;

import java.time.OffsetDateTime;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

class DateConverterTest {

    private final DateConverter sut = new DateConverter();

    @Test
    void convertToAttributeConvertsOffsetDateTimeToDate() {
        final OffsetDateTime value = OffsetDateTime.now();
        assertEquals(new Date(value.toInstant().toEpochMilli()), sut.convertToAttribute(value));
    }

    @Test
    void convertToAttributeConvertsLiteralToDate() {
        final Date date = new Date();
        final String value = date.toInstant().toString();
        assertEquals(date, sut.convertToAttribute(Literal.from(value, XSD.DATETIME)));
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
