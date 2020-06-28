package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.vocabulary.XSD;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;

class DateTimeParameterValueTest {

    @Test
    void getQueryStringReturnsISOFormatDateTimeForLocalDateTime() {
        final LocalDateTime value = LocalDateTime.now();
        final DateTimeParameterValue sut = new DateTimeParameterValue(value);
        final String expected = "\"" + value + "\"^^<" + XSD.DATETIME + ">";
        assertEquals(expected, sut.getQueryString());
    }

    @Test
    void getQueryStringReturnsISOFormatDateTimeForZonedDateTime() {
        final OffsetDateTime value = OffsetDateTime.now();
        final DateTimeParameterValue sut = new DateTimeParameterValue(value);
        final String expected = "\"" + value + "\"^^<" + XSD.DATETIME + ">";
        assertEquals(expected, sut.getQueryString());
    }

    @Test
    void getQueryStringReturnsISOFormatDateTimeForInstant() {
        final Instant value = Instant.now();
        final DateTimeParameterValue sut = new DateTimeParameterValue(value);
        final String expected = "\"" + value + "\"^^<" + XSD.DATETIME + ">";
        assertEquals(expected, sut.getQueryString());
    }

    @Test
    void getQueryStringReturnsISOFormatDateTimeForDate() {
        final Date value = new Date();
        final DateTimeParameterValue sut = new DateTimeParameterValue(value);
        final String expected = "\"" + value.toInstant() + "\"^^<" + XSD.DATETIME + ">";
        assertEquals(expected, sut.getQueryString());
    }
}
