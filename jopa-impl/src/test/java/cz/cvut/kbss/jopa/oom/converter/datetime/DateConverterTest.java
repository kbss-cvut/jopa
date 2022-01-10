package cz.cvut.kbss.jopa.oom.converter.datetime;

import cz.cvut.kbss.jopa.vocabulary.XSD;
import cz.cvut.kbss.ontodriver.model.Literal;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.Date;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.jupiter.api.Assertions.*;

class DateConverterTest {

    private final DateConverter sut = new DateConverter();

    @Test
    void convertToAxiomValueConvertsDateToOffsetDateTimeAtUTC() {
        final Date value = new Date();
        final Object result = sut.convertToAxiomValue(value);
        assertThat(result, instanceOf(OffsetDateTime.class));
        assertEquals(value.toInstant().atOffset(ZoneOffset.UTC), result);
    }

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
